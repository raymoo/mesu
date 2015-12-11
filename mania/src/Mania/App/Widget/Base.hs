{-|
Module      : Mania.App.Widget.Base
Description : Base widget system
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mania.App.Widget.Base ( ScreenSize(..)
                             , WidgetContext(..)
                             , wcTimeStep
                             , wcRawEvents
                             , wcEvents
                             , wcAppContext
                             , wcScreenSize
                             , KeyMotion(..)
                             , ClickData(..)
                             , ClickMotion(..)
                             , WidgetEventKey(..)
                             , Widget
                             , compileWidget
                             , holdWidget
                             , widgetPicture
                             ) where

import Reflex

import Mania.App
import Mania.Picture

import Control.Monad
import Control.Lens

import Control.Monad.RWS.Strict

import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.List (foldl')

import qualified SDL as SDL

import Linear
import Linear.Affine

import qualified Data.Dependent.Map as DM
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Compare.TH


type ScreenSize = V2 Int

data WidgetPiece t =
  WidgetPicture (Behavior t Picture)


newtype WidgetBuilder t =
  WidgetBuilder { runWidgetBuilder :: [WidgetPiece t] -> [WidgetPiece t] }

instance Monoid (WidgetBuilder t) where
  WidgetBuilder f `mappend` WidgetBuilder g = WidgetBuilder (f . g)
  mempty = WidgetBuilder id


singleton :: WidgetPiece t -> WidgetBuilder t
singleton piece = WidgetBuilder (piece :)


listToBuild :: [WidgetPiece t] -> WidgetBuilder t
listToBuild pieces = WidgetBuilder (pieces ++)


buildPieces :: WidgetBuilder t -> [WidgetPiece t]
buildPieces (WidgetBuilder f) = f []


-- | A key event. 'KeyHeld' is for events where a held key causes extra events.
data KeyMotion = KeyPressed
               | KeyReleased
               | KeyHeld
               deriving (Show, Eq)


data ClickMotion = ButtonPressed
                  | ButtonReleased
                  deriving (Show, Eq)


data ClickData = ClickData { _clickMotion :: ClickMotion
                           , _clickPos :: Point V2 Int
                           }


-- | Lists given by events are in reverse chronological order.
data WidgetEventKey t where
  KeyboardEvent :: SDL.Keycode -> WidgetEventKey [KeyMotion]
  ClickEvent :: SDL.MouseButton -> WidgetEventKey [ClickData]
  ResizeEvent :: WidgetEventKey [V2 Int]


deriving instance Show (WidgetEventKey t)
deriving instance Eq (WidgetEventKey t)


deriveGEq ''WidgetEventKey

deriveGCompare ''WidgetEventKey


instance Ord (WidgetEventKey t) where
  compare (KeyboardEvent kc1) (KeyboardEvent kc2) = compare kc1 kc2
  compare (ClickEvent mb1) (ClickEvent mb2) = compare mb1 mb2
  compare ResizeEvent ResizeEvent = EQ


-- | Currently supports keyboard and mouse events.
makeEventMap :: [SDL.Event] -> DM.DMap WidgetEventKey
makeEventMap = foldl' go DM.empty
  where go oldMap (SDL.Event _ (SDL.KeyboardEvent keyData)) =
          let keyEvent
                | SDL.keyboardEventRepeat keyData = KeyHeld
                | otherwise = case (SDL.keyboardEventKeyMotion keyData) of
                               SDL.Released -> KeyReleased
                               SDL.Pressed -> KeyPressed
          in DM.insertWith' (++)
             (KeyboardEvent . SDL.keysymKeycode . SDL.keyboardEventKeysym $ keyData)
             [keyEvent] 
             oldMap
        go oldMap (SDL.Event _ (SDL.MouseButtonEvent buttonData)) =
          let buttonMotion =
                case SDL.mouseButtonEventMotion buttonData of
                 SDL.Released -> ButtonReleased 
                 SDL.Pressed -> ButtonPressed
              clickPos = fmap fromIntegral $ SDL.mouseButtonEventPos buttonData
          in DM.insertWith' (++)
             (ClickEvent . SDL.mouseButtonEventButton $ buttonData)
             [ClickData buttonMotion clickPos]
             oldMap
        go oldMap (SDL.Event _ (SDL.WindowResizedEvent resizeData)) =
          DM.insertWith' (++)
          ResizeEvent
          [fmap fromIntegral $ SDL.windowResizedEventSize resizeData]
          oldMap
        go oldMap _ = oldMap


data WidgetContext t =
  WidgetContext { _wcTimeStep :: Event t TimeStep
                , _wcRawEvents :: Event t [SDL.Event]
                , _wcEvents :: EventSelector t WidgetEventKey
                , _wcAppContext :: AppContext t
                , _wcScreenSize :: Dynamic t ScreenSize
                }

makeLenses ''WidgetContext


mkWidgetContext :: (Reflex t, MonadHold t m) =>
                   AppContext t -> ScreenSize -> m (WidgetContext t)
mkWidgetContext appContext ss = do
  let events = fan . fmap makeEventMap $ _appSDLEvents appContext
      resizes = select events ResizeEvent
  screenSize <- holdDyn ss (fmapMaybe listToMaybe resizes)
  return
    WidgetContext { _wcTimeStep = _appTimeStep appContext
                  , _wcRawEvents = _appSDLEvents appContext
                  , _wcEvents = events
                  , _wcAppContext = appContext
                  , _wcScreenSize = screenSize
                  }


newtype Widget t m a =
  Widget { runWidget :: RWST (WidgetContext t) (WidgetBuilder t) () m a }
  deriving (Functor, Applicative, Monad
           , MonadTrans
           , MonadReader (WidgetContext t) , MonadWriter (WidgetBuilder t)
           , MonadFix )

instance (Reflex t, MonadSample t m) => MonadSample t (Widget t m) where
  sample b = Widget (lift (sample b))

instance (Reflex t, MonadHold t m) => MonadHold t (Widget t m) where
  hold i e = Widget (lift (hold i e))


compileWidgetPiece :: WidgetPiece t -> Behavior t Picture
compileWidgetPiece (WidgetPicture pic) = pic


compileWidget :: (Reflex t, MonadHold t m) =>
                 AppContext t -> ScreenSize -> Widget t m a -> m (Behavior t Picture, a)
compileWidget appContext scrSize (Widget widget) = do
  widgetContext <- mkWidgetContext appContext scrSize
  (res, _, builder) <- runRWST widget widgetContext ()
  let picList = map compileWidgetPiece $ buildPieces builder
  return (fmap pictures (sequence picList), res)


-- | A switching widget
holdWidget :: forall t m a. (Reflex t, MonadHold t m) =>
           Widget t (Widget t m) a -> Event t (Widget t (PushM t) a) -> Widget t m (Dynamic t a)
holdWidget initial newWidgets = do
  wContext <- ask
  scrSize <- view wcScreenSize >>= sample . current
  let appContext = _wcAppContext wContext
      compile = compileWidget appContext scrSize
  (initialPicBehavior, initialRes) <- compile initial
  let evPicAndResult = pushAlways (\new -> compileWidget appContext scrSize new) newWidgets
  let (evPicBehavior, evRes) = splitE evPicAndResult
  picBehavior <- switcher initialPicBehavior evPicBehavior
  let pic = WidgetPicture picBehavior
  tell $ singleton pic
  holdDyn initialRes evRes


-- | Gives you a 'Widget' that will account for the screen size and give a picture
-- behavior.
widgetPicture :: Monad m => (Behavior t Picture) -> Widget t m ()
widgetPicture wPiece = writer
  ((), singleton (WidgetPicture wPiece))
