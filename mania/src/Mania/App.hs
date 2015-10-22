{-# LANGUAGE RankNTypes #-}
module Mania.App (
                   SDLApp(..)
                 , Title(..)
                 , SDLDriver(..)
                 , Quit(..)
                 , runSDLApp
                 , quitEvent
                 ) where


import Reflex
import Reflex.Host.Class

import Mania.Picture
import qualified SDL as SDL

import Control.Monad
import Control.Monad.IO.Class

import Data.Dependent.Sum

import Control.Monad.Fix

import Data.IORef
import Data.Maybe

import Control.Monad.Loops


type SDLApp t m =
  (Reflex t, MonadHold t m, MonadFix m) => Event t [SDL.Event] -> m (SDLDriver t)
  
type Title = String

data Quit = Quit
          deriving (Show, Eq, Ord)

data SDLDriver t =
  SDLDriver { _driverImage :: Behavior t Picture
            , _driverQuit :: Event t Quit
            } 

runSDLApp :: SDL.Renderer -> (forall t m. SDLApp t m) -> IO ()
runSDLApp renderer app = runSpiderHost $ do
  (inputEvent, inputTriggerRef) <- newEventWithTriggerRef

  driver <- runHostFrame $ app inputEvent

  quitHandle <- subscribeEvent $ _driverQuit driver

  let picture = _driverImage driver

  let mainLoop = do
        shouldQuit <- liftIO SDL.pollEvents >>= handleTrigger quitCheck inputTriggerRef

        SDL.clear renderer
        runHostFrame (sample picture) >>= liftIO . renderPicture renderer
        SDL.present renderer
        
        return shouldQuit
      quitCheck = fmap isJust $ readEvent quitHandle

  untilM_ (return ()) mainLoop


  where handleTrigger phase trigger e = do
          mETrigger <- liftIO $ readIORef trigger
          case mETrigger of
           Nothing -> fireEventsAndRead [] phase
           Just eTrigger -> fireEventsAndRead [eTrigger :=> e] phase


quitEvent :: Reflex t => Event t [SDL.Event] -> Event t Quit
quitEvent = fmap (const Quit) . ffilter (any (\ev -> SDL.eventPayload ev == SDL.QuitEvent))
