{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (empty, optional, (<**>), (<|>))
import Control.Lens (iforM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List.NonEmpty (NonEmpty, some1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Options.Applicative (ParserInfo, execParser, help, helper, info, long, short, strArgument, strOption)
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath (dropExtensions, (<.>), (</>))
import RIO.Map (Map)
import qualified RIO.Map as M
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Vector.Boxed as V
import Text.Read (readMaybe)

default (T.Text)

newtype InputSize = InputSize {getInputSize :: [Int]}
  deriving (Read, Show, Eq, Generic)

-- | Graded lex-ordering
instance Ord InputSize where
  compare (InputSize ls) (InputSize rs) =
    comparing sum ls rs <> compare ls rs

instance ToField InputSize where
  toField (InputSize [i]) = toField i
  toField (InputSize is) = toField $ show is

instance FromField InputSize where
  parseField fld =
    InputSize
      <$> ( pure <$> parseField fld
              <|> parseRead (BS.unpack fld)
          )

parseRead :: Read a => String -> Parser a
parseRead = maybe empty pure . readMaybe

data BenchName = BenchName
  { caseName :: !T.Text
  , inputSize :: InputSize
  , implName :: !T.Text
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromField BenchName where
  parseField inp = do
    [a, b, c] <- T.splitOn "/" <$> parseField inp
    ints <-
      InputSize
        <$> ( pure <$> parseRead (T.unpack b)
                <|> parseRead (T.unpack b)
            )
    pure $ BenchName a ints c

instance ToField BenchName where
  toField BenchName {inputSize = InputSize ss, ..} =
    BS.intercalate
      "/"
      [ toField caseName
      , toField $ case ss of
          [i] -> show i
          _ -> show ss
      , toField implName
      ]

data GaugeEntry = GEntry
  { name :: !BenchName
  , mean, meanLB, meanUB, stddev, stddevLB, stddevUB :: Scientific
  }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving anyclass (FromRecord, ToRecord, FromNamedRecord, ToNamedRecord, DefaultOrdered)

{-
>>> src <- LBS.readFile "rims/bench-results/diffupto.csv"
>>> decode @GaugeEntry HasHeader src
-}

data OutputRow = ORow
  { size :: !Int
  , times :: !(M.Map ImplName Scientific)
  }
  deriving (Read, Show, Eq, Ord, Generic)

instance ToNamedRecord OutputRow where
  toNamedRecord ORow {..} =
    namedRecord ["size" .= size]
      <> toNamedRecord times

data Opts = Opts
  { inputFile :: !FilePath
  , moutputPath :: !(Maybe FilePath)
  , targets :: NonEmpty ImplName
  }
  deriving (Read, Show, Eq, Ord, Generic)

optP :: ParserInfo Opts
optP = info (p <**> helper) mempty
  where
    p =
      Opts <$> strArgument (help "Input path")
        <*> optional (strOption (short 'o' <> long "output"))
        <*> some1 (strOption (short 't' <> long "target"))

main :: IO ()
main = do
  Opts {..} <- execParser optP
  let outputPath = fromMaybe (dropExtensions inputFile) moutputPath
  src <- LBS.readFile inputFile
  gauges <- either fail (pure . fmap toRow . foldMap toMap) $ decode HasHeader src
  createDirectoryIfMissing True outputPath
  iforM_ gauges $ \csName rows -> do
    let oPath =
          outputPath </> T.unpack csName <.> "csv"
        keys = foldMap (M.keysSet . times) rows
    LBS.writeFile oPath $ encodeByName (V.fromList $ "size" : map T.encodeUtf8 (filter (`Set.member` keys) $ NE.toList targets)) rows
  pure ()

type ImplName = T.Text

type CaseName = T.Text

toMap ::
  GaugeEntry ->
  MMap.MonoidalMap CaseName (MMap.MonoidalMap InputSize (Map ImplName Scientific))
toMap GEntry {name = BenchName {..}, ..} =
  MMap.singleton caseName $ MMap.singleton inputSize $ Map.singleton implName mean

toRow ::
  MMap.MonoidalMap InputSize (Map ImplName Scientific) ->
  [OutputRow]
toRow =
  map (\(i, c) -> ORow {size = sum $ getInputSize i, times = c})
    . MMap.toAscList