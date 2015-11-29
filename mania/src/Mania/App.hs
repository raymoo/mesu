{-|
Module      : Mania.App
Description : SDL FRP app
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE RankNTypes #-}
module Mania.App (
                   SDLApp(..)
                 , Title(..)
                 , AppContext(..)
                 , SDLDriver(..)
                 , Quit(..)
                 , TimeStep(..)
                 , runSDLApp
                 , quitEvent
                 , time
                 ) where


import Reflex
import Reflex.Host.Class

import Mania.Picture
import qualified SDL as SDL

import Control.Monad
import Control.Monad.IO.Class

import Control.Monad.Reader

import Data.Dependent.Sum

import Control.Monad.Fix

import Data.IORef
import Data.Maybe

import Control.Monad.Loops


-- | Time advancement tick in seconds
type TimeStep = Double

type SDLApp t m =
  (Reflex t, MonadHold t m, MonadFix m) =>
  AppContext t -> m (SDLDriver t)

data AppContext t =
  AppContext { _appTimeStep :: Event t TimeStep 
             , _appSDLEvents :: Event t [SDL.Event]
             }
  
type Title = String

data Quit = Quit
          deriving (Show, Eq, Ord)

data SDLDriver t =
  SDLDriver { _driverImage :: Behavior t Picture
            , _driverQuit :: Event t Quit
            } 

runSDLApp :: SDL.Renderer -> (forall t m. SDLApp t m) -> IO ()
runSDLApp renderer app = runSpiderHost $ do

  timeRef <- liftIO . newIORef =<< SDL.time
  
  (inputEvent, inputTriggerRef) <- newEventWithTriggerRef
  (tickEvent, tickTriggerRef) <- newEventWithTriggerRef

  let context =
        AppContext { _appTimeStep = tickEvent
                   , _appSDLEvents = inputEvent
                   }

  driver <- runHostFrame $ app context

  quitHandle <- subscribeEvent $ _driverQuit driver

  let picture = _driverImage driver

  let mainLoop = do

        newTime <- SDL.time
        oldTime <- liftIO $ readIORef timeRef

        liftIO $ writeIORef timeRef (newTime)
  
        evs <- SDL.pollEvents

        mInputWaiter <- handleTrigger inputTriggerRef evs
        mTickWaiter <- handleTrigger tickTriggerRef (newTime - oldTime)
        
        shouldQuit <- fireEventsAndRead (catMaybes [mInputWaiter, mTickWaiter]) quitCheck

        SDL.clear renderer
        runHostFrame (sample picture) >>= liftIO . renderPicture renderer
        SDL.present renderer
        
        return shouldQuit
      quitCheck = fmap isJust $ readEvent quitHandle

  untilM_ (return ()) mainLoop


  where handleTrigger trigger e = do
          mETrigger <- liftIO $ readIORef trigger
          case mETrigger of
           Nothing -> return Nothing
           Just eTrigger -> return . Just $  eTrigger :=> e


quitEvent :: Reflex t => Event t [SDL.Event] -> Event t Quit
quitEvent = fmap (const Quit) . ffilter (any (\ev -> SDL.eventPayload ev == SDL.QuitEvent))


time :: (Reflex t, MonadHold t m, MonadFix m) => Event t TimeStep -> m (Dynamic t Double)
time steps = foldDyn (+) 0 steps

