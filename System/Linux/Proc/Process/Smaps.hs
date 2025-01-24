{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Linux.Proc.Process.Smaps
  ( Smap (..)
  , formatSmap
  , parseSmaps
  , parseSmap
  , readProcSmaps
  ) where

import           Control.Error (runExceptT, throwE)

import           Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfInput, endOfLine)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Data.Text as Text
import           Data.Word (Word64)

import           System.Linux.Proc.Errors
import           System.Linux.Proc.IO
import           System.Linux.Proc.Process
import           System.Linux.Proc.Process.Maps

import           Text.Printf (printf)

-- | A struct to contain information parsed from the `/proc/PID/smaps` file.
-- Fields that are listed as being in kilobytes in the proc filesystem are
-- converted to bytes.
data Smap = Smap
  { smapMap :: !Map
  , smapSize :: !Word64
  , smapRss :: !Word64
  , smapPss :: !Word64
  , smapPss_Dirty :: !Word64
  , smapShared_Clean :: !Word64
  , smapShared_Dirty :: !Word64
  , smapPrivate_Clean :: !Word64
  , smapPrivate_Dirty :: !Word64
  , smapReferenced :: !Word64
  , smapAnonymous :: !Word64
  , smapKSM :: !Word64
  , smapLazyFree :: !Word64
  , smapAnonHugePages :: !Word64
  , smapShmemPmdMapped :: !Word64
  , smapFilePmdMapped :: !Word64
  , smapShared_Hugetlb :: !Word64
  , smapPrivate_Hugetlb :: !Word64
  , smapSwap :: !Word64
  , smapSwapPss :: !Word64
  , smapKernelPageSize :: !Word64
  , smapMMUPageSize :: !Word64
  , smapLocked :: !Word64
  , smapTHPeligible :: !Int
  , smapProtectionKey :: !Int
  , smapVmFlags :: ![ByteString]
  }
  deriving (Eq, Ord, Show)

-- | Format an @Smap@ as a @ByteString@ using the same format as an `smaps` file.
formatSmap :: Smap -> ByteString
formatSmap Smap {..} =
  BS.empty -- FIXME

-- | Parse an `smaps` file.
parseSmaps :: Parser [Smap]
parseSmaps = Atto.many' parseSmap <* endOfInput

-- | Parse a block from an `smaps` file.
parseSmap :: Parser Smap
parseSmap = do
  let isSpace' c = c == ' ' || c == '\t'
      skipSpace' = Atto.skipWhile isSpace'
  smapMap <- parseMap <* endOfLine
  smapSize <- "Size:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapKernelPageSize <- "KernelPageSize:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapMMUPageSize <- "MMUPageSize:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapRss <- "Rss:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapPss <- "Pss:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapPss_Dirty <- "Pss_Dirty:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapShared_Clean <- "Shared_Clean:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapShared_Dirty <- "Shared_Dirty:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapPrivate_Clean <- "Private_Clean:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapPrivate_Dirty <- "Private_Dirty:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapReferenced <- "Referenced:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapAnonymous <- "Anonymous:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapKSM <- "KSM:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapLazyFree <- "LazyFree:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapAnonHugePages <- "AnonHugePages:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapShmemPmdMapped <- "ShmemPmdMapped:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapFilePmdMapped <- "FilePmdMapped:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapShared_Hugetlb <- "Shared_Hugetlb:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapPrivate_Hugetlb <- "Private_Hugetlb:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapSwap <- "Swap:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapSwapPss <- "SwapPss:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapLocked <- "Locked:" *> skipSpace' *> parseUnitsValue <* endOfLine
  smapTHPeligible <- "THPeligible:" *> skipSpace' *> decimal <* endOfLine
  smapProtectionKey <- "ProtectionKey:" *> skipSpace' *> decimal <* endOfLine
  smapVmFlags <- "VmFlags:" *> skipSpace' *> (BS.words <$> Atto.takeTill (== '\n')) <* endOfLine
  pure Smap {..}

-- | Parse a value with following units in the format `[kMG]B`.
parseUnitsValue :: Parser Word64
parseUnitsValue = do
  value <- decimal <* char ' '
  unit <- Atto.choice multipliers <* char 'B'
  pure $ unit * value
  where
    multipliers = zipWith (\n c -> n <$ char c) (iterate (1024*) 1024) ("kMG" :: String)

-- | Read the `/proc/PID/smaps` file and return a list of `Smap` structures.
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcSmaps :: ProcessId -> IO (Either ProcError [Smap])
readProcSmaps pid = do
  let fp = fpSmapsFor pid
      mkErr = ProcParseError fp . Text.pack
      parseOnlyM p = either (throwE . mkErr) pure . Atto.parseOnly p
  runExceptT $ parseOnlyM parseSmaps =<< readProcFile fp

-- A `printf` pattern for the filepath of the maps file of a given PID
fpSmapsFor :: ProcessId -> FilePath
fpSmapsFor (ProcessId n) = printf "/proc/%d/smaps" n
