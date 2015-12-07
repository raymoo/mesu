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
                             , wcScreenWidth
                             , wcScreenHeight
                             , KeyMotion(..)
                             , ClickData(..)
                             , ClickMotion(..)
                             , WidgetEventKey(..)
                             , Widget
                             , compileWidget
                             , holdWidget
                             , widgetPicture
                             , absolutePicture
                             ) where

import Reflex

import Mania.App
import Mania.Picture

import Control.Monad
import Control.Lens

import Control.Monad.RWS.Strict

import Data.Monoid

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
  WidgetPicture (ScreenSize -> Behavior t Picture)


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


data WidgetEventKey t where
  KeyboardEvent :: SDL.Keycode -> WidgetEventKey [KeyMotion]
  -- ^ Chooses the event that gives lists of key events in reverse chronological order
  ClickEvent :: SDL.MouseButton -> WidgetEventKey [ClickData]


deriving instance Show (WidgetEventKey t)
deriving instance Eq (WidgetEventKey t)
deriving instance Ord (WidgetEventKey t)


deriveGEq ''WidgetEventKey

deriveGCompare ''WidgetEventKey


-- | Currently supports keyboard events only. 
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
        go oldMap _ = oldMap


data WidgetContext t =
  WidgetContext { _wcTimeStep :: Event t TimeStep
                , _wcRawEvents :: Event t [SDL.Event]
                , _wcEvents :: EventSelector t WidgetEventKey
                , _wcAppContext :: AppContext t
                , _wcScreenWidth :: Int
                , _wcScreenHeight :: Int
                }

makeLenses ''WidgetContext


mkWidgetContext :: Reflex t => AppContext t -> ScreenSize -> WidgetContext t
mkWidgetContext appContext (V2 w h) =
  WidgetContext { _wcTimeStep = _appTimeStep appContext
                , _wcRawEvents = _appSDLEvents appContext
                , _wcEvents = fan . fmap makeEventMap $ _appSDLEvents appContext
                , _wcAppContext = appContext
                , _wcScreenWidth = w
                , _wcScreenHeight = h
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


compileWidgetPiece :: ScreenSize -> WidgetPiece t -> Behavior t Picture
compileWidgetPiece scrSize (WidgetPicture f) = f scrSize


compileWidget :: (Reflex t, Monad m) =>
                 AppContext t -> ScreenSize -> Widget t m a -> m (Behavior t Picture, a)
compileWidget appContext scrSize (Widget widget) = do
  (res, _, builder) <- runRWST widget (mkWidgetContext appContext scrSize) ()
  let picList = map (compileWidgetPiece scrSize) $ buildPieces builder
  return (fmap pictures (sequence picList), res)


-- | A switching widget
holdWidget :: forall t m a. (Reflex t, MonadHold t m) =>
           Widget t (Widget t m) a -> Event t (Widget t (PushM t) a) -> Widget t m (Dynamic t a)
holdWidget initial newWidgets = do
  wContext <- ask
  let appContext = _wcAppContext wContext
      scrSize = V2 (_wcScreenWidth wContext) (_wcScreenHeight wContext)
      compile = compileWidget appContext scrSize
  (initialPicBehavior, initialRes) <- compile initial
  let evPicAndResult = pushAlways (\new -> compileWidget appContext scrSize new) newWidgets
  let (evPicBehavior, evRes) = splitE evPicAndResult
  picBehavior <- switcher initialPicBehavior evPicBehavior
  let pic = WidgetPicture (const picBehavior)
  tell $ singleton pic
  holdDyn initialRes evRes


-- | Gives you a 'Widget' that will account for the screen size and give a picture
-- behavior.
widgetPicture :: Monad m => (ScreenSize -> Behavior t Picture) -> Widget t m ()
widgetPicture wPiece = writer
  ((), singleton (WidgetPicture wPiece))


-- | Like 'widgetPicture', but disregarding the size of the screen.
absolutePicture :: Monad m => Behavior t Picture -> Widget t m ()
absolutePicture = widgetPicture . const
