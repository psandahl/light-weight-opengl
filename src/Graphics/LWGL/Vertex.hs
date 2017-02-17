-- |
-- Module: Graphics.LWGL.Vertex
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Vertex
    ( VertexP (..)
    , makeVertexArrayObjectWithVertexP
    ) where

import           Foreign             (Storable (..), castPtr)
import           Linear              (V3)

import           Graphics.LWGL.Api
import           Graphics.LWGL.Types

-- | A vertex record with just one field: position.
data VertexP = VertexP
    { position :: !(V3 GLfloat)
    } deriving Show

instance Storable VertexP where
    sizeOf v = sizeOf $ position v
    alignment v = alignment $ position v
    peek ptr = VertexP <$> (peek $ castPtr ptr)
    poke ptr v = poke (castPtr ptr) $ position v

-- | Create a Vertex Array Object, with the specified BufferUsage and the
-- given vertices. The vertex attributes are populated (location = 0). At the
-- return the Vertex Array Object is still bound.
makeVertexArrayObjectWithVertexP :: BufferUsage -> [VertexP]
                                 -> IO VertexArrayObject
makeVertexArrayObjectWithVertexP bufferUsage vertices = do
    [vao] <- glGenVertexArray 1
    glBindVertexArray vao

    [vbo] <- glGenBuffers 1
    glBindBuffer ArrayBuffer vbo
    glBufferDataList ArrayBuffer vertices bufferUsage

    -- Setting position - three components of type GLfloat
    glEnableVertexAttribArray (Location 0)
    glVertexAttribPointer (Location 0) Three GLFloat False 0 0

    return vao
