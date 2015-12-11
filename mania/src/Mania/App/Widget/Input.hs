{-|
Module      : Mania.App.Widget.Input
Description : Base provides some input primitives, but here are some nicer ones.
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Mania.App.Widget.Input (
                                -- * Keyboard
                                keyEvents
                              , keyPresses
                              , keyReleases
                              , keyEntered
                                -- * Mouse
                              , clicks
                              , clickArea
                                -- * Misc
                              , filterListEvent
                              ) where

import Reflex
import Mania.App.Widget.Base
import Control.Lens

import Linear
import Linear.Affine

import qualified SDL as SDL


-- | Takes a 'SDL.Keycode' and gives an event that fire whenever the key was
-- pressed or released. The list is in "chronological" order.
keyEvents :: Reflex t => SDL.Keycode -> Widget t (Event t [KeyMotion])
keyEvents kc = do
  reversedEvents <- fmap (\selector -> select selector (KeyboardEvent kc)) $ view wcEvents
  return $ fmap reverse reversedEvents -- The original event list is in reverse order


-- | Filters elements out of a list event, and then only fires if it ends up
--  being nonempty.
filterListEvent :: Reflex t => (a -> Bool) -> Event t [a] -> Event t [a]
filterListEvent p event = ffilter (not . null) $ fmap (filter p) event


filterKeyType :: Reflex t => (a -> Bool) -> Event t [a] -> Event t Int
filterKeyType p event = fmap length $ filterListEvent p event


-- | Returns the number of press events occured
keyPresses :: Reflex t => SDL.Keycode -> Widget t (Event t Int)
keyPresses = fmap (filterKeyType (== KeyPressed)) . keyEvents 


-- | Returns the number of release events occured
keyReleases :: Reflex t => SDL.Keycode -> Widget t (Event t Int)
keyReleases = fmap (filterKeyType (== KeyReleased)) . keyEvents


-- | Returns number of times the key was "inputted", including when it was held.
keyEntered :: Reflex t => SDL.Keycode -> Widget t (Event t Int)
keyEntered = fmap (filterKeyType (`elem` [KeyPressed, KeyHeld])) . keyEvents


inRegion :: (Num a, Ord a) => SDL.Rectangle a -> Point V2 a -> Bool
inRegion (SDL.Rectangle (P (V2 x y)) (V2 w h)) (P (V2 x' y')) =
  x <= x' && x' < x + w && y <= y' && y' < y + h


-- | When there were one or more clicks of a button, chronological order
clicks :: Reflex t => SDL.MouseButton -> Widget t (Event t [ClickData])
clicks button = do
  selector <- view wcEvents
  return $ fmap reverse (select selector (ClickEvent button))


-- | Listen for clicks in a specific area
clickArea :: Reflex t =>
             SDL.Rectangle Int -> SDL.MouseButton -> Widget t (Event t [ClickData])
clickArea region button = filterListEvent goodClickData <$> clicks button
  where goodClickData clickData = inRegion region $ _clickPos clickData
