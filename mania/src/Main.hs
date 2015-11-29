{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Mania.Picture
import Mania.App

import Reflex

import qualified SDL as SDL

import Linear
import Linear.Affine

windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow

main :: IO ()
main = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "Mania" windowConfig
  renderer <- SDL.createRenderer window (-1) $ SDL.RendererConfig SDL.AcceleratedVSyncRenderer True

  loadedBG <- SDL.loadBMP "bg.bmp"

  bg <- SDL.createTextureFromSurface renderer loadedBG
  
  runSDLApp renderer (mainApp bg)
  SDL.quit


posSin :: Floating a => a -> a
posSin a = (sin a + 1) / 2


mainApp :: SDL.Texture -> SDLApp t m
mainApp tex AppContext { _appTimeStep = steps, _appSDLEvents = evs } = do
  curPic <- picture
  return $ SDLDriver curPic (quitEvent evs)
  where background t =
          colorFill . fmap floor $ (V4 (posSin (5*t) * 255)  (posSin (3*t - 0.2) * 255) (posSin (17*t + 0.3) * 255) 0)
        title = textureBox tex (SDL.Rectangle (P $ V2 100 100) (V2 500 400))
        picture = do
          timeDyn <- time steps
          bgDyn <- mapDyn background timeDyn
          picDyn <- dynList [bgDyn, constDyn title]
          fmap current $ mapDyn pictures picDyn

dynList :: (Reflex t, MonadHold t m) => [Dynamic t a] -> m (Dynamic t [a])
dynList dyns = do
  singleton'd <- traverse (mapDyn (:[])) dyns
  mconcatDyn singleton'd

                  
