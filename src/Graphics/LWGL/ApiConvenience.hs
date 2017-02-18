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
    ) where

import           Control.Monad       (unless)
import           Foreign             (Storable (sizeOf), withArray)

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
