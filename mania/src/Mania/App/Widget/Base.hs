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
{-# LANGUAGE RoleAnnotations #-}
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
                             , widgetPicture
                             ) where

import Reflex
import Reflex.Monad
import Reflex.Monad.ReaderWriter
import Reflex.Monad.ReflexM

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

newtype WidgetPiece t =
  WidgetPicture { unWidgetPiece :: (Behavior t [Picture]) }
  deriving (Monoid)


instance Reflex t => Switching t (WidgetPiece t) where
  switching (WidgetPicture initial) ev =
    let underEv = fmap unWidgetPiece ev
    in fmap WidgetPicture (switching initial underEv)


instance (Reflex t) => SwitchMerge t (WidgetPiece t) where
  switchMerge initMap mapEvents =
    fmap WidgetPicture $
    switchMerge (fmap unWidgetPiece initMap) ((fmap.fmap.fmap) unWidgetPiece mapEvents)


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


newtype Widget t a =
  Widget { runWidget :: ReaderWriterT (WidgetContext t) (WidgetPiece t) (ReflexM t) a }
  deriving (Functor, Applicative, Monad
           , MonadReader (WidgetContext t) , MonadWriter (WidgetPiece t)
           , MonadFix
           )

instance Reflex t => MonadSample t (Widget t) where
  sample b = Widget (lift (sample b))

instance Reflex t => MonadHold t (Widget t) where
  hold i e = Widget (lift (hold i e))

instance Reflex t => MonadSwitch t (Widget t) where
  switchM updatesma = Widget (switchM $ fmap runWidget updatesma)
  switchMapM mapUpdates = Widget (switchMapM $ fmap runWidget mapUpdates)

compileWidgetPiece :: Reflex t => WidgetPiece t -> Behavior t Picture
compileWidgetPiece (WidgetPicture pic) = fmap pictures pic


compileWidget :: (Reflex t, MonadHold t m, MonadFix m) =>
                 AppContext t -> ScreenSize -> Widget t a -> m (Behavior t Picture, a)
compileWidget appContext scrSize (Widget widget) = do
  widgetContext <- mkWidgetContext appContext scrSize
  (res, piece) <- runReflexM $ runReaderWriterT widget widgetContext
  let picList = compileWidgetPiece piece
  return (picList, res)


-- | Gives you a 'Widget' that will account for the screen size and give a picture
-- behavior.
widgetPicture :: Reflex t => (Behavior t Picture) -> Widget t ()
widgetPicture wPiece = writer
  ((), WidgetPicture (fmap (:[]) wPiece))
