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
                     , textureFill
                     , textureBox
                     , colorFill
                     , colorBox
                     , pictures
                     , renderPicture
                     ) where

import qualified SDL as SDL

import Linear

import Foreign.C

import Data.Word

import Data.StateVar

import Data.Foldable


-- | Renderable pictures
data Picture =
  DrawBoxed SDL.Texture (Maybe (SDL.Rectangle CInt))
  | DrawRect (V4 Word8) (Maybe (SDL.Rectangle CInt))
  | DrawLayered [Picture]


-- | Draw a texture against the whole screen
textureFill :: SDL.Texture -> Picture
textureFill tex = DrawBoxed tex Nothing

-- | Draw a texture in a specified box
textureBox :: SDL.Texture -> SDL.Rectangle Int -> Picture
textureBox tex = DrawBoxed tex . Just . fmap fromIntegral

colorFill :: V4 Word8 -> Picture
colorFill color = DrawRect color Nothing

-- | Draw a primitive colored rectangle
colorBox :: V4 Word8 -> SDL.Rectangle Int -> Picture
colorBox color rect = DrawRect color . Just . fmap fromIntegral $ rect


-- | Draw multiple pictures. First element gets drawn on the bottom.
pictures :: [Picture] -> Picture
pictures = DrawLayered


-- | Use a renderer to render a picture
renderPicture :: SDL.Renderer -> Picture -> IO ()
renderPicture renderer (DrawBoxed tex mRect) =
  SDL.copy renderer tex Nothing mRect
renderPicture renderer (DrawRect color mRect) = do
  SDL.rendererDrawColor renderer $= color
  SDL.drawRect renderer mRect
renderPicture renderer (DrawLayered pics) =
  traverse_ (renderPicture renderer) pics
