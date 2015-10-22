{-|
Module      : Mania.Picture
Description : SDL-drawable things
Copyright   : (c) Leon Medvinsky, 2015

License     : GPL-3
Maintainer  : lmedvinsky@hotmail.com
Stability   : experimental
Portability : portable
-}

module Mania.Picture (
                       Picture
                     , drawTexture
                     , renderPicture
                     ) where

import qualified SDL as SDL

import Linear


-- | Renderable pictures
data Picture =
  DrawImage SDL.Texture


drawTexture :: SDL.Texture -> Picture
drawTexture = DrawImage


renderPicture :: SDL.Renderer -> Picture -> IO ()
renderPicture renderer (DrawImage tex) =
  SDL.copy renderer tex Nothing Nothing
