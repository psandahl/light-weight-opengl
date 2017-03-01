-- |
-- Module: Graphics.LWGL.Vertex_P_Tex
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Vertex_P_Tex
    ( Vertex (..)
    , makeVertexArrayObject
    ) where

import           Foreign             (Storable (..), castPtr, plusPtr)
import           Linear              (V2, V3)

import           Graphics.LWGL.Api
import           Graphics.LWGL.Mesh
import           Graphics.LWGL.Types

-- | A vertex record with two fields, position and texCoord.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving Show

instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (texCoord v)

    alignment v = alignment $ position v

    peek ptr = do
        let pPtr = castPtr ptr
        pos <- peek pPtr
        let tPtr = castPtr $ pPtr `plusPtr` sizeOf pos
        tex <- peek tPtr
        return Vertex {position = pos, texCoord = tex}

    poke ptr v = do
        let pPtr = castPtr ptr
            tPtr = castPtr $ ptr `plusPtr` sizeOf (position v)
        poke pPtr $ position v
        poke tPtr $ texCoord v

instance Meshable Vertex where
    fromList bufferTarget =
        makeVertexArrayObject . setBufferFromList bufferTarget

    fromVector bufferTarget =
        makeVertexArrayObject . setBufferFromVector bufferTarget

-- | Create a Vertex Array Object, with the specified BufferUsage and the
-- given vertices. The vertex attributes are populated with position at
-- (location = 0) and texCoord at (location = 1). At the
-- return the Vertex Array Object is still bound.
makeVertexArrayObject :: IO Vertex -> IO VertexArrayObject
makeVertexArrayObject setBuffer = do
    [vaoId] <- glGenVertexArray 1
    glBindVertexArray vaoId

    [vboId] <- glGenBuffers 1
    glBindBuffer ArrayBuffer vboId
    v <- setBuffer

    -- Setting position - three components of type GLfloat
    glEnableVertexAttribArray (AttributeIndex 0)
    glVertexAttribPointer (AttributeIndex 0) Three GLFloat False (sizeOf v) 0

    -- Setting texCoord - two components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 1)
    glVertexAttribPointer (AttributeIndex 1) Two GLFloat False
                          (sizeOf v) (sizeOf $ position v)

    return vaoId
