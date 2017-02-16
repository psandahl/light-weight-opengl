-- |
-- Module: Graphics.LWGL.Texture
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Texture
    ( readImageRGB8
    , readImageRGBA8
    ) where

import           Codec.Picture

readImageRGB8 :: FilePath -> IO (Either String (Image PixelRGB8))
readImageRGB8 file = (fmap convertRGB8) <$> readImage file

readImageRGBA8 :: FilePath -> IO (Either String (Image PixelRGBA8))
readImageRGBA8 file = (fmap convertRGBA8) <$> readImage file
