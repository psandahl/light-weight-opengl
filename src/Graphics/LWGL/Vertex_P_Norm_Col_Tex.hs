-- |
-- Module: Graphics.LWGL.Vertex_P_Norm_Col_Tex
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Vertex_P_Norm_Col_Tex
    ( Vertex (..)
    , makeVertexArrayObject
    ) where

import           Foreign             (Storable (..), castPtr, plusPtr)
import           Linear              (V2, V3)

import           Graphics.LWGL.Api
import           Graphics.LWGL.Mesh
import           Graphics.LWGL.Types

-- | A vertex record with four fields, position, normal, color and texCoord.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , normal   :: !(V3 GLfloat)
    , color    :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving Show

instance Storable Vertex where
    sizeOf v = sizeOf (position v) +
               sizeOf (normal v) +
               sizeOf (color v) +
               sizeOf (texCoord v)

    alignment v = alignment $ position v

    peek ptr = do
        let pPtr = castPtr ptr
        pos <- peek pPtr
        let nPtr = castPtr $ pPtr `plusPtr` sizeOf pos
        norm <- peek nPtr
        let cPtr = castPtr $ nPtr `plusPtr` sizeOf norm
        col <- peek cPtr
        let tPtr = castPtr $ cPtr `plusPtr` sizeOf col
        tex <- peek tPtr
        return Vertex { position = pos
                      , normal = norm
                      , color = col
                      , texCoord = tex
                      }

    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr $ pPtr `plusPtr` sizeOf (position v)
            cPtr = castPtr $ nPtr `plusPtr` sizeOf (normal v)
            tPtr = castPtr $ cPtr `plusPtr` sizeOf (color v)
        poke pPtr $ position v
        poke nPtr $ normal v
        poke cPtr $ color v
        poke tPtr $ texCoord v

instance Meshable Vertex where
    fromList bufferTarget =
        makeVertexArrayObject . setBufferFromList bufferTarget

    fromVector bufferTarget =
        makeVertexArrayObject . setBufferFromVector bufferTarget

-- | Create a Vertex Array Object, with the specified buffer filling function.
-- The vertex attributes are populated with position at (location = 0),
-- normal at (location = 1), color at (location = 2) and texCoord at
-- (location = 3).
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

    -- Setting normal - three components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 1)
    glVertexAttribPointer (AttributeIndex 1) Three GLFloat False
                          (sizeOf v) (sizeOf $ position v)

    -- Setting normal - three components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 2)
    glVertexAttribPointer (AttributeIndex 2) Three GLFloat False
                          (sizeOf v) $ sizeOf (position v) + sizeOf (normal v)

    -- Setting texCoord - two components of type GLfloat.
    glEnableVertexAttribArray (AttributeIndex 3)
    glVertexAttribPointer (AttributeIndex 3) Two GLFloat False
                          (sizeOf v) $ sizeOf (position v) +
                                       sizeOf (normal v) +
                                       sizeOf (color v)

    return vaoId
