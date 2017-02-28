-- |
-- Module: Graphics.LWGL.Vertex_P_Norm_Tex
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Vertex_P_Norm_Tex
    ( Vertex (..)
    , makeVertexArrayObject
    ) where

import           Foreign                      (Storable (..), castPtr, plusPtr)
import           Linear                       (V2, V3)

import           Graphics.LWGL.Api
import           Graphics.LWGL.ApiConvenience
import           Graphics.LWGL.Mesh
import           Graphics.LWGL.Types

-- | A vertex record with three fields, position, normal and texCoord.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , normal   :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving Show

instance Storable Vertex where
    sizeOf v = (sizeOf $ position v) +
               (sizeOf $ normal v) +
               (sizeOf $ texCoord v)
    alignment v = alignment $ position v
    peek = error "Not implemented"
    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr $ ptr `plusPtr` (sizeOf $ position v)
            tPtr = castPtr $ nPtr `plusPtr` (sizeOf $ normal v)
        poke pPtr $ position v
        poke nPtr $ normal v
        poke tPtr $ texCoord v

instance Meshable Vertex where
    fromList = makeVertexArrayObject

-- | Create a Vertex Array Object, with the specified BufferUsage and the
-- given vertices. The vertex attributes are populated with position at
-- (location = 0), normal at (location = 1) and texCoord at (location = 2).
-- At the return the Vertex Array Object is still bound.
makeVertexArrayObject :: BufferUsage -> [Vertex] -> IO VertexArrayObject
makeVertexArrayObject bufferUsage vertices = do
    [vaoId] <- glGenVertexArray 1
    glBindVertexArray vaoId

    [vboId] <- glGenBuffers 1
    glBindBuffer ArrayBuffer vboId
    bufferDataList ArrayBuffer vertices bufferUsage

    let v = head vertices

    -- Setting position - three components of type GLfloat
    glEnableVertexAttribArray (AttributeIndex 0)
    glVertexAttribPointer (AttributeIndex 0) Three GLFloat False (sizeOf v) 0

    -- Setting normal - three components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 1)
    glVertexAttribPointer (AttributeIndex 1) Three GLFloat False
                          (sizeOf v) $ (sizeOf $ position v)

    -- Setting texCoord - two components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 2)
    glVertexAttribPointer (AttributeIndex 2) Two GLFloat False
                          (sizeOf v) $ (sizeOf $ position v) + (sizeOf $ normal v)

    return vaoId
