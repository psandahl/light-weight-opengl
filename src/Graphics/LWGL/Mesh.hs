-- |
-- Module: Graphics.LWGL.Mesh
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Mesh
    ( Mesh (..)
    , Meshable (..)
    , buildFromList
    , buildFromVector
    , setBufferFromList
    , setBufferFromVector
    ) where

import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as Vec
import           Foreign                      (Storable)

import           Graphics.LWGL.ApiConvenience
import           Graphics.LWGL.Types

data Mesh = Mesh
    { vao         :: !VertexArrayObject
    , numVertices :: !Int
    , indices     :: !(Vector GLuint)
    } deriving Show

setBufferFromList :: Storable a => BufferUsage -> [a] -> IO a
setBufferFromList bufferUsage vertices = do
    bufferDataList ArrayBuffer vertices bufferUsage
    return $ head vertices

setBufferFromVector :: Storable a => BufferUsage -> Vector a -> IO a
setBufferFromVector bufferUsage vertices = do
    bufferDataVector ArrayBuffer vertices bufferUsage
    return $ Vec.head vertices

class Storable a => Meshable a where
    fromList :: BufferUsage -> [a] -> IO VertexArrayObject
    fromVector :: BufferUsage -> Vector a -> IO VertexArrayObject

buildFromList :: (Storable a, Meshable a) => BufferUsage -> [a] -> [GLuint] -> IO Mesh
buildFromList bufferUsage vertices indices' = do
    vaoId <- fromList bufferUsage vertices
    return Mesh { vao         = vaoId
                , numVertices = length vertices
                , indices     = Vec.fromList indices'
                }

buildFromVector :: (Storable a, Meshable a)
                => BufferUsage -> Vector a -> Vector GLuint -> IO Mesh
buildFromVector bufferUsage vertices indices' = do
    vaoId <- fromVector bufferUsage vertices
    return Mesh { vao         = vaoId
                , numVertices = Vec.length vertices
                , indices     = indices'
                }
