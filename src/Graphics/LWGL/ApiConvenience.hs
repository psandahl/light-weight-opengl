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
    , bufferDataVector
    , drawTrianglesList
    , drawTrianglesVector
    , setMatrix4
    , setVector3
    ) where

import           Control.Monad        (unless)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import           Foreign              (Storable (sizeOf), castPtr, with,
                                       withArray)
import           Linear               (M44, V3)

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

-- | Convenience function of 'glBufferData' where data is provided in a vector.
bufferDataVector :: Storable a => BufferTarget -> Vector a -> BufferUsage -> IO ()
bufferDataVector bufferTarget xs bufferUsage =
    unless (Vec.null xs) $
        Vec.unsafeWith xs $ \ptr -> do
            let first = Vec.head xs
                itemSize = sizeOf first
                storageSize = itemSize * Vec.length xs
            glBufferData bufferTarget storageSize ptr bufferUsage

-- | Convenience function of 'glDrawElements' where triangles are drawn from
-- from GLuint indices provided in a list.
drawTrianglesList :: [GLuint] -> IO ()
drawTrianglesList xs =
    unless (null xs) $
        withArray xs $ \ptr -> do
            let numElems = length xs
            glDrawElements Triangles numElems IdxUnsignedInt ptr

-- | Convenience function of 'glDrawElements' where triangles are drawn from
-- from GLuint indices provided in a Vector.
drawTrianglesVector :: Vector GLuint -> IO ()
drawTrianglesVector xs =
    unless (Vec.null xs) $
        Vec.unsafeWith xs $ \ptr -> do
            let numElems = Vec.length xs
            glDrawElements Triangles numElems IdxUnsignedInt ptr

-- | Set a M44 uniform value.
setMatrix4 :: Location -> M44 GLfloat -> IO ()
setMatrix4 location matrix =
    with matrix $ glUniformMatrix4fv location 1 True . castPtr

-- | Set a V3 uniform value.
setVector3 :: Location -> V3 GLfloat -> IO ()
setVector3 location vector =
    with vector $ glUniform3fv location 1 . castPtr
