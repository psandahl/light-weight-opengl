-- |
-- Module: Graphics.LWGL.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Types
    ( ClearBufferMask (..)
    , ShaderType (..)
    , Program (..)
    , ToBitfield (..)
    , ToEnum (..)
    , GLbitfield
    , GLboolean
    , GLchar
    , GLenum
    , GLfloat
    , GLsizei
    , GLuint
    , combineBits
    ) where

import           Data.Bits   (zeroBits, (.|.))
import           Data.List   (foldl')
import           Graphics.GL (GLbitfield, GLboolean, GLchar, GLenum, GLfloat,
                              GLsizei, GLuint)
import qualified Graphics.GL as GL

-- | Class that help types to expose bitfield values.
class ToBitfield a where
    toBitfield :: a -> GLbitfield

-- | Class that help types to expost enum values.
class ToEnum a where
    toEnum :: a -> GLenum

-- | Mask values to indicate which buffers to clear.
data ClearBufferMask
    = ColorBuffer
    | DepthBuffer
    | StencilBuffer
    deriving Show

instance ToBitfield ClearBufferMask where
    toBitfield ColorBuffer   = GL.GL_COLOR_BUFFER_BIT
    toBitfield DepthBuffer   = GL.GL_DEPTH_BUFFER_BIT
    toBitfield StencilBuffer = GL.GL_STENCIL_BUFFER_BIT

data ShaderType
    = ComputeShader
    | VertexShader
    | TessControlShader
    | TessEvaluationShader
    | GeometryShader
    | FragmentShader
    deriving Show

instance ToEnum ShaderType where
    toEnum ComputeShader        = GL.GL_COMPUTE_SHADER
    toEnum VertexShader         = GL.GL_VERTEX_SHADER
    toEnum TessControlShader    = GL.GL_TESS_CONTROL_SHADER
    toEnum TessEvaluationShader = GL.GL_TESS_EVALUATION_SHADER
    toEnum GeometryShader       = GL.GL_GEOMETRY_SHADER
    toEnum FragmentShader       = GL.GL_FRAGMENT_SHADER

-- | Representation of a linked shader program.
newtype Program = Program GLuint
    deriving Show

combineBits :: ToBitfield a => [a] -> GLbitfield
combineBits mask = foldl' (\a v -> a .|. (toBitfield v)) zeroBits mask
