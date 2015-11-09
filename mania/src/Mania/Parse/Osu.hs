{-|
Module      : Mania.Parse.Osu
Description : Osu! Beatmap file parsing
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mania.Parse.Osu (
                       ) where

import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T


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
data OsuEvent = OsuEvent
              deriving (Show)

data OsuTempo = NewBPM Int
              | VelMult Int
              deriving (Show)


data TimingPoint = TimingPoint
  { _tpTime :: Int
  , _tpTempo :: OsuTempo
  , _tpMetre :: Int
  , _tpSampleset :: Int
  , _tpCustomSampleset :: Int
  , _tpVolume :: Int -- Percentage
  , _tpUnknown :: () -- TODO: Figure this out
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
  , _hoUnknown :: () -- TODO: Figure this out
  , _hoTime :: Int
  , _hoType :: NoteType
  , _hoSEffects :: [SoundEffect]
  , _hoExtra :: Text
  }
               deriving (Show)

