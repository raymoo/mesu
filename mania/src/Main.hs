module Main (main) where

import Mania.Sound

import qualified Sound.PortAudio as PA

import Control.Concurrent


main :: IO ()
main = do
  PA.withPortAudio app >> return ()


app :: IO (Either a ())
app = do
  track <- loadFile "test.ogg"
  Right (dev,_) <- PA.getDefaultInputInfo
  playTrackWithCB print (putStrLn "Done!!!!") dev track
  threadDelay 1000000
  stopTrack track
  return $ Right ()

