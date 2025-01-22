{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Linux.Proc.Process.Maps
  ( Map (..)
  , formatMap
  , parseMaps
  , parseMap
  , readProcMaps
  ) where

import           Control.Applicative ((<|>), optional)
import           Control.Error (runExceptT, throwE)

import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, hexadecimal)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Data.Bits ((.&.), (.|.))
import           Data.Bool (bool)
import qualified Data.Text as Text
import           Data.Word (Word8, Word16, Word64)

import           System.Linux.Proc.Errors
import           System.Linux.Proc.IO
import           System.Linux.Proc.Process

import           Text.Printf (printf)

-- | A struct to contain information parsed from the `/proc/PID/maps` file.
data Map = Map
  { mapAddress :: !(Word64, Word64)
  , mapOffset :: !Word64
  , mapDev :: !(Word16, Word16)
  , mapInode :: !Word64
  , mapPerms :: !Word8
  , mapShared :: !Bool
  , mapPathname :: !ByteString
  }
  deriving (Eq, Ord, Show)

-- | Format a @Map@ as a @ByteString@ using the same format as a `maps` file.
formatMap :: Map -> ByteString
formatMap Map {..} =
  let col1 =
        hex 8 (fst mapAddress) <> "-" <> hex 8 (snd mapAddress) <> " " <>
        perms mapPerms mapShared <> " " <>
        hex 8 mapOffset <> " " <>
        hex 2 (fst mapDev) <> ":" <> hex 2 (snd mapDev) <> " " <>
        dec mapInode
      col2 = mapPathname
      hex n = BS.pack . printf "%0*x" (n :: Int)
      dec = BS.pack . printf "%d"
      pad n bs = bs <> BS.replicate (n - BS.length bs) ' '
      w = if BS.null col2 then 0 else 72
      perms n s = BS.pack
        [ bool '-' 'r' (n .&. 4 /= 0)
        , bool '-' 'w' (n .&. 2 /= 0)
        , bool '-' 'x' (n .&. 1 /= 0)
        , bool 'p' 's' s
        ]
   in pad w col1 <> " " <> col2

-- | Parse a `maps` file.
parseMaps :: Parser [Map]
parseMaps = Atto.many' $ parseMap <* endOfLine

-- | Parse a line from a `maps` file.
parseMap :: Parser Map
parseMap = do
  let isSpace' c = c == ' ' || c == '\t'
      skipSpace' = Atto.skipWhile isSpace'
  mapAddress <- (,) <$> hexadecimal <* "-" <*> hexadecimal <* skipSpace'
  mapPerms <- parsePerms
  mapShared <- parseShared <* skipSpace'
  mapOffset <- hexadecimal <* skipSpace'
  mapDev <- (,) <$> hexadecimal <* ":" <*> hexadecimal <* skipSpace'
  mapInode <- decimal <* optional skipSpace'
  mapPathname <- Atto.takeTill (== '\n')
  pure Map {..}

-- | Parse the first three characters of a perms field from a `maps` file.
parsePerms :: Parser Word8
parsePerms = do
  r <- (== 'r') <$> (char 'r' <|> char '-')
  w <- (== 'w') <$> (char 'w' <|> char '-')
  x <- (== 'x') <$> (char 'x' <|> char '-')
  pure $ bool 0 4 r .|. bool 0 2 w .|. bool 0 1 x

-- | Parse the last character of a perms field from a `maps` file.
parseShared :: Parser Bool
parseShared = (== 's') <$> (char 's' <|> char 'p')

-- | Read the `/proc/PID/maps` file and return a list of `Map` structures.
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMaps :: ProcessId -> IO (Either ProcError [Map])
readProcMaps pid = do
  let fp = fpMapsFor pid
      mkErr = ProcParseError fp . Text.pack
      parseOnlyM p = either (throwE . mkErr) pure . Atto.parseOnly p
  runExceptT $ parseOnlyM parseMaps =<< readProcFile fp

-- A `printf` pattern for the filepath of the maps file of a given PID
fpMapsFor :: ProcessId -> FilePath
fpMapsFor (ProcessId n) = printf "/proc/%d/maps" n
