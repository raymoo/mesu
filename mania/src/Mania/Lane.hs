{-|
Module      : Mania.Lane
Description : Lanes as in a beatmania map. Each one corresponds to one key.
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : RankNTypes, RecordWildCards, FlexibleContexts, MultiWayIf
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Mania.Lane ( 
                    ManElement(..)
                  , LNType(..)
                  , KiaiMode(..)
                  , BarType(..)
                  , LaneEvent(..)
                  , IncomingElements(..)
                  , CurrentElements(..)
                  , LaneInput(..)
                  , Lane(..)
                  , LaneSettings(..)
                  , TimingSettings(..)
                  , mkLane
                  , advanceLane
                  , processInput

                    -- * FRP stuff
                  , ScoreElem(..)
                  , laneFRP
                  ) where

import qualified Data.Sequence as Seq
import Data.Sequence ((><))
import Data.List (sortOn)

import Control.Monad.State
import Control.Monad.Tardis

import Data.Foldable
import Data.Either

import Control.Arrow

import Control.Monad.Writer

import Reflex

import Data.Maybe


-- | Settings for timing
data TimingSettings t =
  TimingSettings { timingRadius :: t -- | How long before and after a thing can be hit
                 } 


-- | Settings for display time etc.
data LaneSettings t =
  LaneSettings { lsCutoff :: t
               , lsWindow :: t
               }
  deriving (Show, Eq, Ord)

-- | Possible control things that can happen
data LaneInput =
  LanePressed
  | LaneReleased
  deriving (Show, Eq, Ord)
  

-- | Possible states for kiai
data KiaiMode = NoKiai | Kiai
              deriving (Show, Eq, Ord)

-- | Mania elements. Parametrized on type of time.
data ManElement t =
  Note                    -- ^ Standard one-press note
  | LongNote t (LNType t) -- ^ A hold. t argument is the end time
  | KiaiChange KiaiMode   -- ^ Change of kiai
  | TempoBar BarType      -- ^ Bars that come down for timing help
  deriving (Show, Eq, Ord)


data LNType t =
  LNFresh      -- ^ A new hold, not pressed yet
  | LNHeld t   -- ^ A currently held hold. The first argument is when it started being held.
  | LNDead t t -- ^ Finished hold. First and second arguments are the start/end hold times.
    deriving (Show, Eq, Ord)

data BarType =
  Thick
  | Thin
  deriving (Show, Eq, Ord)

-- | Represents an occurence of an element
type LaneEvent t = (t, ManElement t)


startTime :: LaneEvent t -> t
startTime = fst

endTime :: LaneEvent t -> t
endTime (_, LongNote t _) = t
endTime (t,_) = t


-- | Stack of incoming elements. Occurence times are increasing
type IncomingElements t = [LaneEvent t]


-- | Sequence of currently active (on the screen/hittable) elements
type CurrentElements t = Seq.Seq (LaneEvent t)


-- | A Lane, again parametrized on time type.
data Lane t =
  Lane { laneIncoming :: IncomingElements t
       , laneCurrent :: CurrentElements t
       , laneWindowSize :: t                -- ^ How long in advance the player sees notes
       , laneCutoff :: t                    -- ^ How long before offscreen elements are killed
       }
  deriving (Show, Eq, Ord)


-- | Gets the new elements and the remaining elements at the position of the
-- beginning of the player's view window
extractNewElements :: Ord t => t -> IncomingElements t ->
                      ([LaneEvent t], IncomingElements t)
extractNewElements windowStart incoming = span (\ev -> startTime ev <= windowStart) incoming


-- | Puts new elements into the current elements
addNewElements :: [LaneEvent t] -> CurrentElements t -> CurrentElements t
addNewElements newElems currElems = currElems >< Seq.fromList newElems


-- | Gets rid of the old elements of the current elements. Cutoff time is the time
-- elements should be inactive.
removeOldElements :: Ord t => t -> CurrentElements t -> ([LaneEvent t], CurrentElements t)
removeOldElements cutoff currElems = (foldMap (:[]) discardedElems, newSeq)
  where (discardedElems, newSeq) = Seq.spanl (\ev -> endTime ev < cutoff) currElems


-- | Advance a Lane to the next time. Note that this CANNOT be used to seek
-- backwards, as going forwards discards no longer active elements. It outputs
-- the discarded elements in addition to the new lane, in case they are needed
-- for scoring purposes. The time input is the new time.
advanceLane :: (Ord t, Num t) => t -> Lane t -> ([LaneEvent t], Lane t)
advanceLane newTime oldLane@Lane{..} = (deadElems, oldLane{ laneIncoming = newIncoming
                                                          , laneCurrent = newCurr
                                                          })
  where cutoff = newTime - laneCutoff
        visibleStart = newTime + laneWindowSize
        (newActive, newIncoming) = extractNewElements visibleStart laneIncoming
        (deadElems, newCurr) = removeOldElements cutoff
                               . addNewElements newActive
                               $ laneCurrent


-- | Takes a start time and a list of lane events to construct a lane.
mkLane :: (Ord t, Num t) => LaneSettings t -> t -> [LaneEvent t] -> Lane t
mkLane LaneSettings{..} startTime evs =
  snd $ advanceLane startTime Lane { laneIncoming = sortOn fst evs
                                   , laneCurrent = Seq.empty
                                   , laneWindowSize = lsWindow
                                   , laneCutoff = lsCutoff
                                   }


-- | Takes the current time and the kind of input, and updates a lane. Also possibly
-- outputting some lane elements that were hit.
processInput :: (Num t, Ord t) => TimingSettings t -> t -> LaneInput -> Lane t ->
                ([LaneEvent t], Lane t)
processInput TimingSettings{..} evTime LanePressed oldLane =
  (killed, oldLane { laneCurrent = tooEarly >< Seq.fromList newEligList >< tooLate })
  where (possiblyEligible, tooLate) =
          Seq.spanl (\(t,_) -> t <= evTime + timingRadius) (laneCurrent oldLane)
        (eligible, tooEarly) =
          Seq.spanr (\(t,_) -> t >= evTime - timingRadius) possiblyEligible
        (killed, newEligList) =
          partitionEithers . toList $
          changeMinOn (pressable . snd) fst pSelector Right eligible
        pSelector le = case fmap (pressElem evTime) le of
                        (t, Right me) -> Right (t, me)
                        (t, Left me) -> Left (t, me)
processInput TimingSettings{..} evTime LaneReleased oldLane =
  (killed, oldLane { laneCurrent = tooEarly >< Seq.fromList newEligList >< tooLate })
  where (possiblyEligible, tooLate) =
          Seq.spanl (\le -> endTime le <= evTime + timingRadius) (laneCurrent oldLane)
        (eligible, tooEarly) =
          Seq.spanr (\le -> endTime le >= evTime - timingRadius) possiblyEligible
        (killed, newEligList) =
          partitionEithers . toList $
          changeMinOn (releasable . snd) fst pSelector Right eligible
        pSelector le = case fmap (releaseElem evTime) le of
                        (t, Right me) -> Right (t, me)
                        (t, Left me) -> Left (t, me)
                                                          

-- | Presses an element, possibly being removed. Also takes
-- in the time.
pressElem :: t -> ManElement t -> Either (ManElement t) (ManElement t)
pressElem _ Note = Left Note
pressElem hold (LongNote end LNFresh) = Right $ LongNote end (LNHeld hold)
pressElem _ x = Right x


-- | Releases an element, possibly being removed. Also takes
-- in the time.
releaseElem :: t -> ManElement t -> Either (ManElement t) (ManElement t)
releaseElem unhold (LongNote end (LNHeld hold)) = Right $ LongNote end (LNDead hold unhold)
releaseElem _ x = Right x


-- | Is the element pressable
pressable :: ManElement t -> Bool
pressable Note = True
pressable (LongNote _ LNFresh) = True
pressable _ = False


-- | Is the element releasable
releasable :: ManElement t -> Bool
releasable (LongNote _ (LNHeld _)) = True
releasable _ = False


-- | Maps the minimal elements of a 'Traversable' one way, and everything else
-- another way. The first argument is a predicate that checks if something is
-- valid for minimum checking. The second is the thing to compare on. The third
-- is how to map minimal elements, and the fourth is how to map other things.
changeMinOn :: (Traversable t, Ord b) => (a -> Bool) -> (a -> b) -> (a -> c) -> (a -> c)
               -> t a -> t c
changeMinOn p view f g as = flip evalTardis (Nothing,Nothing) $ traverse cMO as
  where mMin Nothing mb = mb
        mMin ma Nothing = ma
        mMin (Just a) (Just b) = Just $ min a b
        mLT (Just _) Nothing  = True
        mLT (Just a) (Just b) = a < b
        mLT _        _        = False
        cMO a = do
          let viewed = view a
          if not (p a)
            then return $ g a
            else do
            modifyForwards (mMin $ Just viewed)
            pastMin <- getPast
            futMin <- getFuture        
            modifyBackwards (mMin $ Just viewed)
            return $ if Just viewed == min pastMin futMin then f a else g a


data ScoreElem t =
  NoteScore t -- ^ Accuracy - timing differential
  | FreshLongNote -- ^ LongNote that was never pressed
  | LongNoteScore t t -- ^ Accuracy of both the press and release


laneFRP :: (Reflex s, MonadHold s m, MonadFix m, Num t, Ord t) =>
           TimingSettings t -> Dynamic s t -> Event s [LaneInput] -> Lane t ->
           m (Dynamic s (Lane t), Event s ([ScoreElem t]))
laneFRP timeSets times inputs initial = do
  let bothEvs =
        appendEvents (fmap (singList . Right) (updated times)) ((fmap.fmap) Left inputs)
      processBoth anEv (_,lane) = do
        curTime <- sample (current times)
        return $ processInputsOrTime timeSets curTime anEv lane
      scoresAndLane =
        foldDynM processBoth ([],initial) bothEvs
  laneDyn <- scoresAndLane >>= mapDyn snd
  return (laneDyn, undefined)
  where singList = (:[])


makeScoreElem :: Num t => t -> LaneEvent t -> Maybe (ScoreElem t)
makeScoreElem hitTime (noteTime, Note) = Just $ NoteScore (hitTime - noteTime)
makeScoreElem hitTime (noteTime, LongNote endTime LNFresh) = Just FreshLongNote
makeScoreElem hitTime (noteTime, LongNote endTime (LNHeld startHold)) = Just $
                          LongNoteScore (startHold - noteTime) (hitTime - endTime)
makeScoreElem _ _ = Nothing


processInputsOrTime :: (Ord t, Num t) =>
                 TimingSettings t -> t -> [Either LaneInput t] -> Lane t -> ([ScoreElem t], Lane t)
processInputsOrTime timeSets hitTime inputs lane =
  first catMaybes $ foldr processOne ([],lane) inputs
  where processOne (Left input) (prevScores,lane) =
          let (hitElems, newLane) = processInput timeSets hitTime input lane
          in (map (makeScoreElem hitTime) hitElems ++ prevScores, newLane)
        processOne (Right newTime) (prevScores, lane) =
          let (deadElems, newLane) = advanceLane newTime lane
          in (map (makeScoreElem newTime) deadElems ++ prevScores, newLane)



