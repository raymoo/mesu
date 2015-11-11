{-|
Module      : Mania.Parse.Osu
Description : Osu! Beatmap file parsing
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Mania.Parse.Osu (
                       ) where

import qualified Data.Attoparsec.Text as AP
import Data.Attoparsec.Text (Parser)

import Data.Text (Text)
import qualified Data.Text as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Either (rights)

import Control.Applicative

import Data.Char
import Data.Bits


data GeneralData = GeneralData
  { _gdAudioFileName :: Text
  , _gdAudioLeadIn :: Int    -- TODO: Check if this can be non-int in other files
  , _gdPreviewTime :: Int
  , _gdCountdown :: Int      -- TODO: Check for non-int
  , _gdSampleSet :: Text
  , _gdStackLeniency :: Double
  , _gdMode :: Int
  , _gdLetterboxInBreaks :: Int
  }
                 deriving (Show)


data EditorData = EditorData
  { _edBookmarks :: Int
  , _edDistanceSpacing :: Int
  , _edBeatDivisor :: Int
  , _edGridSize :: Int
  }
                deriving (Show)


data Metadata = Metadata
  { _mdTitle :: Text
  , _mdTitleUnicode :: Text
  , _mdArtist :: Text
  , _mdArtistUnicode :: Text
  , _mdCreator :: Text
  , _mdVersion :: Text
  , _mdSource :: Text
  , _mdTags :: [Text]
  , _mdBeatmapID :: Int
  , _mdBeatmapSetID :: Int
  }
              deriving (Show)


data DifficultyData = DifficultyData
  { _ddHPDrainRate :: Int
  , _ddCircleSize :: Int
  , _ddOverallDifficulty :: Int
  , _ddApproachRate :: Int
  , _ddSliderMultiplier :: Double
  , _ddSliderTickRate :: Double
  }
                    deriving (Show)


-- TODO: Determine how Osu video/background events are formatted
data VBGEvent = VBGEvent
              deriving (Show)


data OsuTempo = NewBPM Int
              | VelMult Double
              deriving (Show)


data TimingPoint = TimingPoint
  { _tpTime :: Int
  , _tpTempo :: OsuTempo
  , _tpMetre :: Int
  , _tpSampleset :: Int
  , _tpCustomSampleset :: Int
  , _tpVolume :: Int -- Percentage
  , _tpUnknown :: Int -- TODO: Figure this out
  , _tpKiai :: Bool
  }
                 deriving (Show)


type TimingPoints = [TimingPoint]


data NoteType = NTNormal -- Code is "1"
              | NTLongWeird -- Code is "2". Contains some tempo data.
              | NTNormalWeird -- Code is "5". Nothing else known to distinguish from "1"
              | NTLong -- Code is "128"
              deriving (Show)


data SoundEffect = SEClap | SEFinish | SEWhistle
                 deriving (Show)
  

data HitObject = HitObject
  { _hoColumn :: Int
  , _hoUnknown :: Int -- TODO: Figure this out
  , _hoTime :: Int
  , _hoType :: NoteType
  , _hoSEffects :: [SoundEffect]
  , _hoExtra :: Text
  }
               deriving (Show)

type SettingsBlock = HashMap Text Text


comment :: Parser Text
comment = fmap T.pack $ AP.string "//" *> many (AP.satisfy $ AP.notInClass "\n\r")


-- | Parses things of the form setting:value
parseSBLine :: Parser (Text, Text)
parseSBLine =
  (,) <$> (AP.takeWhile1 isAlpha) <*>
  (AP.char ':' *> many (AP.char ' ') *> AP.takeWhile (AP.notInClass "\n\r"))


-- | Parses the part after the section header of normal settings sections
parseSBContent :: Parser SettingsBlock
parseSBContent = fmap (foldr (uncurry HM.insert) HM.empty) setList
  where oneLine = AP.eitherP comment parseSBLine
                  <|> fail "Expected setting or comment"
                  
        setsAndComms = (oneLine `AP.sepBy` AP.skipSpace)
                       
        setList = fmap rights setsAndComms


doubleToTempo :: Double -> OsuTempo
doubleToTempo x
  | x <= 0 = VelMult (-x)
  | otherwise = NewBPM (round $ 60 / (x / 1000)) -- 60 seconds, 1000 ms in a s


-- | Parses a block of TimingPoints (after the header)
parseTimingPoints :: Parser [TimingPoint]
parseTimingPoints = parseTimingPoint `AP.sepBy` AP.skipSpace


-- | Parses a single TimingPoint
parseTimingPoint :: Parser TimingPoint
parseTimingPoint = do
  time <- AP.decimal
  AP.char ','

  tempoOrBpm <- fmap doubleToTempo AP.double
  AP.char ','

  metre <- AP.decimal
  AP.char ','

  sampleset <- AP.decimal
  AP.char ','

  customSampleset <- AP.decimal
  AP.char ','

  volume <- AP.decimal
  AP.char ','

  unknown <- AP.decimal
  AP.char ','

  kiaiNum <- AP.decimal

  kiai <- if
    | kiaiNum == 0 -> return False
    | kiaiNum == 1 -> return True
    | otherwise -> fail "Kiai number neither 0 nor 1"

  return TimingPoint { _tpTime = time
                     , _tpTempo = tempoOrBpm
                     , _tpMetre = metre
                     , _tpSampleset = sampleset
                     , _tpCustomSampleset = customSampleset
                     , _tpVolume = volume
                     , _tpUnknown = unknown
                     , _tpKiai = kiai
                     }


-- | Converts an 'Int' in a bitset format to a list of 'SoundEffect's.
-- clap = 8, finish = 4, whistle = 2
getSoundEffects :: Int -> [SoundEffect]
getSoundEffects x = map snd . filter (\(code,_) -> code `isIn` x) $ sePairs
  where sePairs = [ (8, SEClap), (4, SEFinish), (2, SEWhistle) ]
        bit `isIn` num = bit .&. num /= 0


-- | Parses a block of HitObjects (the part after the section header)
parseHitObjects :: Parser [HitObject]
parseHitObjects = parseHitObject `AP.sepBy` AP.skipSpace

parseHitObject :: Parser HitObject
parseHitObject = do
  colNum <- AP.decimal
  AP.char ','

  unknown <- AP.decimal
  AP.char ','

  time <- AP.decimal
  AP.char ','

  typenum <- AP.decimal
  AP.char ','

  notetype <- case typenum of
    1 -> return NTNormal
    2 -> return NTLongWeird
    5 -> return NTNormalWeird
    128 -> return NTLong
    _ -> fail $ "Unrecognized note type: " ++ show typenum

  soundEffects <- fmap getSoundEffects AP.decimal
  AP.char ','

  extraData <- AP.takeTill (AP.inClass "\r\n")

  return HitObject { _hoColumn = colNum
                   , _hoUnknown = unknown
                   , _hoTime = time
                   , _hoType = notetype
                   , _hoSEffects = soundEffects
                   , _hoExtra = extraData
                   }
