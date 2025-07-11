{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

  -- | Read swapped data back into memory for some or all processes

import Control.Exception (IOException, handle)
import Control.Monad (unless, void, when)
import Data.Binary.Get (getWord64host, runGet)
import Data.Bits ((.&.), testBit)
import Data.Char (intToDigit)
import Data.Foldable (find, for_)
import Data.Maybe (fromMaybe, maybeToList)
import Numeric (floatToDigits)
import Options.Applicative
import System.Linux.Proc.Errors (ProcError (..), renderProcError)
import System.Linux.Proc.Process.Maps
import System.Linux.Proc.Process.Smaps
import System.Linux.Proc.Process (ProcessId (..), getProcProcessIds)
import System.Exit (die)
import System.IO (IOMode (..), SeekMode (..), hSeek, stderr, withBinaryFile)
import Text.Printf (printf)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Console.Terminal.Size as TS

data Options = Options
  { optVerbose :: Bool
  , optNoop :: Bool
  , optProcesses :: [ProcessId]
  } deriving (Show)

main :: IO ()
main = do

  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <- customExecParser
    ( prefs $ columns cols )
    ( info
      ( helper <*> do
          optVerbose <- switch $
            short 'v' <> long "verbose" <>
            help "Produce verbose output"
          optNoop <- switch $
            short 'n' <> long "noop" <>
            help "Just show what would be done"
          optProcesses <- many . fmap ProcessId . argument auto $
            metavar "PID ..." <>
            help "Processes to unswap (default: all processes)"
          pure Options{..}
      )
      ( fullDesc <> header "Description" )
    )

  pids <- if null optProcesses
    then either (die . show) pure =<< getProcProcessIds
    else pure optProcesses

  smapResults <- traverse readProcSmaps pids

  for_ (zip pids smapResults) $ \(pid, result) ->
    case result of
      Left err -> do
        unless (isNotFoundError err) $
          T.hPutStrLn stderr $ renderProcError err
      Right smaps -> do
        let swappedSmaps = filter ((0 /=) . smapSwap) smaps
        unless (null swappedSmaps) $ do
          let totalBytes = sum $ regionSize . mapAddress . smapMap <$> swappedSmaps
              swappedBytes = sum $ smapSwap <$> swappedSmaps
          when optVerbose $
            printf "%s %s bytes of process %d (%s bytes swapped, %.1f%%)\n"
              (if optNoop then "Would read" else "Reading" :: String)
              (showHuman 3 $ fromIntegral totalBytes)
              (unProcessId pid)
              (showHuman 3 $ fromIntegral swappedBytes)
              (fromIntegral swappedBytes / fromIntegral totalBytes * 100 :: Double)
          unless optNoop $
            unswapProcessRegions pid swappedSmaps

isNotFoundError :: ProcError -> Bool
isNotFoundError (ProcReadError _ txt) = "does not exist" `T.isInfixOf` txt
isNotFoundError _ = False

regionSize :: Num n => (n, n) -> n
regionSize (from, to) = to - from

showHuman :: Int -> Double -> String
showHuman precision number = int <> point <> frac <> maybeToList q
  where
    (digits, expnt) = floatToDigits 10 n
    (significant, _rounding) = splitAt precision $ digits <> repeat 0
    (int, frac) = splitAt expnt $ intToDigit <$> significant
    point = if null frac then "" else "."
    n = number / m
    (m, q) = fromMaybe (last scales) $ find ((abs number >=) . fst) scales
    qualifiers = map Just "YZEPTGMK" ++ [Nothing] ++ map Just "munpfazy"
    multipliers = iterate (/1024) (1024^(8::Int))
    scales = zip multipliers qualifiers

-- Find swapped pages via `pagemap` and read the first byte of each via `mem`
unswapProcessRegions :: ProcessId -> [Smap] -> IO ()
unswapProcessRegions (ProcessId pid) regions = do
  handle (print @IOException) $
    withBinaryFile (printf "/proc/%d/pagemap" pid) ReadMode $ \hPagemap ->
      handle (print @IOException) $
        withBinaryFile (printf "/proc/%d/mem" pid) ReadMode $ \hMem ->
          for_ regions $ \Smap { smapMap = Map { mapAddress = region, mapPerms = perms }, smapKernelPageSize = pageSize } -> do
            let addrToMapSeek a = fromIntegral $ a `div` (pageSize `div` 8)
                addrToMemSeek = fromIntegral
                addr = fst region
            -- It's not possible to unswap unreadable regions
            when (perms .&. 4 /= 0) $ do
              hSeek hPagemap AbsoluteSeek . addrToMapSeek $ addr
              pm <- BL.hGet hPagemap . addrToMapSeek $ regionSize region
              let pw = runGet (many getWord64host) pm
              for_ (zip [addr, addr + pageSize ..] pw) $ \(a, w) -> do
                when (testBit w 62) $ do
                  hSeek hMem AbsoluteSeek $ addrToMemSeek a
                  void $ B.hGet hMem 1
