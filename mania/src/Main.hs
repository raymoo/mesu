{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Mania.Picture
import Mania.App
import Mania.App.Widget.Base
import Mania.App.Widget.Input

import Reflex
import Reflex.Monad

import Data.Functor

import qualified SDL as SDL

import Linear
import Linear.Affine

import System.Random

import Control.Monad.Fix

import Data.Word

windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow

main :: IO ()
main = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "Mania" windowConfig
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig SDL.AcceleratedVSyncRenderer True

  runSDLApp renderer mainApp
  SDL.quit


mainApp :: SDLApp t m
mainApp context = do
  (picBeh, _) <- compileWidget context (V2 800 600) colorsWidget
  return $ SDLDriver picBeh (quitEvent . _appSDLEvents $ context)

                  
randomV4s :: (Num a, Random a, RandomGen g) => g -> [V4 a]
randomV4s gen = makeTupList (randoms gen)
  where makeTupList (r:g:b:rest) = V4 r g b 1 : makeTupList rest


randomColors :: (Reflex t, MonadHold t m, MonadFix m) =>
                Int -> Event t a -> m (Event t (V4 Word8))
randomColors seed event = zipListWithEvent const (randomV4s (mkStdGen seed)) event


colorsWidget :: Reflex t => Widget t ()
colorsWidget = do
  datas <-  clicks SDL.ButtonLeft
  randomCols <-
    randomColors 0 $ filterListEvent (\cd -> _clickMotion cd == ButtonPressed) datas
  void $ widgetHold (widgetPicture (pure . colorFill $ V4 0 0 0 255)) (fmap colorWidget randomCols)


-- | Not really necessary here, but used to demonstrate holdWidget
colorWidget :: Reflex t => V4 Word8 -> Widget t ()
colorWidget color = do
  widgetPicture (pure $ colorFill color)
