-- |
-- Module: Graphics.LWGL.Texture
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Texture
    ( TextureFormat (..)
    , loadTexture2D
    , readImageRGB8
    , readImageRGBA8
    ) where

import           Codec.Picture
import qualified Data.Vector.Storable as V

import           Graphics.LWGL.Api
import           Graphics.LWGL.Types

data TextureFormat = RGB8 | RGBA8
    deriving Show

loadTexture2D :: TextureFormat -> Bool -> FilePath -> IO (Either String Texture)
loadTexture2D format useMipmaps file = do
    [texture] <- glGenTextures 1
    glBindTexture Texture2D texture

    eImg <- loadImage2D format file
    case eImg of
        Right () -> do
            glTexParameteri Texture2D TextureWrapS GLRepeat
            glTexParameteri Texture2D TextureWrapT GLRepeat

            if useMipmaps
                then do
                    glGenerateMipmap Texture2D
                    glTexParameteri Texture2D TextureMinFilter GLLinearMipmapLinear
                    glTexParameteri Texture2D TextureMagFilter GLLinear
                else do
                    glTexParameteri Texture2D TextureMinFilter GLLinear
                    glTexParameteri Texture2D TextureMagFilter GLLinear

            return $ Right texture

        Left err -> return $ Left err

loadImage2D :: TextureFormat -> FilePath -> IO (Either String ())
loadImage2D RGB8  = loadImage2DRGB8
loadImage2D RGBA8 = loadImage2DRGBA8

loadImage2DRGB8 :: FilePath -> IO (Either String ())
loadImage2DRGB8 file = do
    eImage <- readImageRGB8 file
    case eImage of
        Right img ->
            V.unsafeWith (imageData img) $ \ptr -> do
                glTexImage2D Texture2D 0 ImgRGB
                             (fromIntegral $ imageWidth img)
                             (fromIntegral $ imageHeight img)
                             PxlRGB PxlUnsignedByte ptr
                return $ Right ()

        Left err -> return $ Left err

loadImage2DRGBA8 :: FilePath -> IO (Either String ())
loadImage2DRGBA8 file = do
    eImage <- readImageRGBA8 file
    case eImage of
        Right img ->
            V.unsafeWith (imageData img) $ \ptr -> do
                glTexImage2D Texture2D 0 ImgRGBA
                             (fromIntegral $ imageWidth img)
                             (fromIntegral $ imageHeight img)
                             PxlRGBA PxlUnsignedByte ptr
                return $ Right ()

        Left err -> return $ Left err

readImageRGB8 :: FilePath -> IO (Either String (Image PixelRGB8))
readImageRGB8 file = (fmap convertRGB8) <$> readImage file

readImageRGBA8 :: FilePath -> IO (Either String (Image PixelRGBA8))
readImageRGBA8 file = (fmap convertRGBA8) <$> readImage file
