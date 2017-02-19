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
    ) where

import           Foreign             (Storable)

import           Graphics.LWGL.Types

data Mesh = Mesh
    { vao         :: !VertexArrayObject
    , numVertices :: !Int
    } deriving Show

class Storable a => Meshable a where
    fromList :: BufferUsage -> [a] -> IO VertexArrayObject

buildFromList :: (Storable a, Meshable a) => BufferUsage -> [a] -> IO Mesh
buildFromList bufferUsage xs = do
    vaoId <- fromList bufferUsage xs
    return Mesh { vao         = vaoId
                , numVertices = length xs
                }
