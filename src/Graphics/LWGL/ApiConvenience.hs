-- |
-- Module: Graphics.LWGL.ApiConvenience
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.ApiConvenience
    ( bufferDataList
    , setMatrix4
    ) where

import           Control.Monad       (unless)
import           Foreign             (Storable (sizeOf), castPtr, with,
                                      withArray)
import           Linear              (M44)

import           Graphics.LWGL.Api
import           Graphics.LWGL.Types

-- | Convenice function of 'glBufferData' where data is provided in a list.
bufferDataList :: Storable a => BufferTarget -> [a] -> BufferUsage -> IO ()
bufferDataList bufferTarget xs bufferUsage =
    unless (null xs) $
        withArray xs $ \ptr -> do
            let first = head xs
                itemSize = sizeOf first
                storageSize = itemSize * length xs
            glBufferData bufferTarget storageSize ptr bufferUsage

-- | Set a M44 uniform value.
setMatrix4 :: Location -> M44 GLfloat -> IO ()
setMatrix4 location matrix =
    with matrix $ glUniformMatrix4fv location 1 True . castPtr
