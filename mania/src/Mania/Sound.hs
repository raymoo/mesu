{-|
Module      : Mania.Sound
Description : An abstraction for music files that can be played and have their
              position queried
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Mania.Sound (
                     Track
                   , TimeCallback
                   , StopCallback
                   , loadFile
                   , getTrackTime
                   , playTrack
                   , playTrackWithCB
                   , stopTrack
                   ) where


import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as Snd

import qualified Sound.PortAudio as PA
import qualified Sound.PortAudio.Base as PA

import Data.IORef

import Control.Applicative

import Foreign.C.Types (CDouble(..), CFloat(..), CULong(..))
import Foreign

newtype Track = Track (IORef (Either StoppedTrack PlayingTrack))


newtype StoppedTrack =
  StoppedTrack Snd.Handle

type FrameCount = CULong
type SampleRate = CULong


data PlayingTrack =
  PlayingTrack Snd.Handle (PA.Stream CFloat CFloat) FrameCount SampleRate


loadFile :: String -> IO Track
loadFile filename = do
  info <- Snd.getFileInfo filename
  handle <- Snd.openFile filename Snd.ReadMode info
  let stopped = Left $ StoppedTrack handle
  fmap Track (newIORef stopped)


mkStreamCb :: Snd.Info -> Snd.Handle -> (FrameCount -> IO ()) -> PA.StreamCallback CFloat CFloat
mkStreamCb info handle recTime timeInfo _ numFrames _ outPtr = do
  readCount <- Snd.hGetBuf handle (castPtr outPtr :: Ptr Float) (fromIntegral numFrames)
  recTime numFrames
  return $ if readCount <= 0 then PA.Complete else PA.Continue


mkFinCb :: StopCallback -> IORef (Either StoppedTrack PlayingTrack) -> PA.FinCallback
mkFinCb stopcb ref = do
  esp <- readIORef ref
  stopcb


-- | Time in seconds from the beginning
getTrackTime :: Track -> IO (Maybe Double)
getTrackTime (Track ref) = do
  esp <- readIORef ref
  case esp of
   Left _ -> return Nothing
   Right (PlayingTrack _ _ time sampRate) -> do
     return . Just $ fromIntegral time / fromIntegral sampRate

type StopCallback = IO ()


-- | Takes a 
type TimeCallback = FrameCount -> IO ()


playTrack :: PA.PaDeviceIndex -> Track -> IO (Maybe PA.Error)
playTrack = playTrackWithCB (const $ return ()) (return ())


playTrackWithCB :: TimeCallback -> StopCallback -> PA.PaDeviceIndex -> Track -> IO (Maybe PA.Error)
playTrackWithCB timecb stopcb device (Track ref) = do
  esp <- readIORef ref
  case esp of
   Left (StoppedTrack handle) -> do
     let hInfo = Snd.hInfo handle
     esInfo <- PA.getDeviceInfo device
     case esInfo of
      Left err -> return (Just err)
      Right dInfo -> do
        let latencySuggestion = PA.defaultLowOutputLatency dInfo
            sampleRate :: Num a => a
            sampleRate = fromIntegral $ Snd.samplerate hInfo
            outParams =
              PA.StreamParameters device (fromIntegral (Snd.channels hInfo)) latencySuggestion

        Snd.hSeek handle Snd.AbsoluteSeek 0
        possiblyStrm <- PA.openStream Nothing
                                      (Just outParams)
                                      sampleRate
                                      Nothing
                                      []
                                      (Just (mkStreamCb hInfo handle (liftA2 (>>) (incTime ref) timecb)))
                                      (Just (mkFinCb stopcb ref))
        case possiblyStrm of
         Left err -> return (Just err)
         Right stream -> do
           writeIORef ref (Right $ PlayingTrack handle stream 0 sampleRate)
           PA.startStream stream
           return Nothing
   Right (PlayingTrack handle stream _ _) -> do
     PA.stopStream stream
     writeIORef ref (Left $ StoppedTrack handle)
     playTrackWithCB timecb stopcb device (Track ref)


stopTrack :: Track -> IO (Maybe PA.Error)
stopTrack (Track ref) = do
  stoppedOrPlaying <- readIORef ref
  case stoppedOrPlaying of
   Left _ -> return Nothing
   Right (PlayingTrack handle stream _ _) -> do
     res <- PA.stopStream stream
     writeIORef ref (Left (StoppedTrack handle))
     PA.closeStream stream
     return res


-- | Increments time of a 'Track'. Don't use outside callback.
incTime :: IORef (Either a PlayingTrack) -> FrameCount -> IO ()
incTime ref dt = do
  value <- readIORef ref
  case value of
   Left _ -> return ()
   Right (PlayingTrack handle stream oldTime sampRate) ->
     writeIORef ref $ Right (PlayingTrack handle stream (dt + oldTime) sampRate)
