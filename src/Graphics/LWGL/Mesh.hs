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

import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import           Foreign              (Storable)

import           Graphics.LWGL.Types

data Mesh = Mesh
    { vao         :: !VertexArrayObject
    , numVertices :: !Int
    , indices     :: !(Vector GLuint)
    } deriving Show

class Storable a => Meshable a where
    fromList :: BufferUsage -> [a] -> IO VertexArrayObject

buildFromList :: (Storable a, Meshable a) => BufferUsage -> [a] -> [GLuint] -> IO Mesh
buildFromList bufferUsage vertices indices' = do
    vaoId <- fromList bufferUsage vertices
    return Mesh { vao         = vaoId
                , numVertices = length vertices
                , indices     = Vec.fromList indices'
                }
