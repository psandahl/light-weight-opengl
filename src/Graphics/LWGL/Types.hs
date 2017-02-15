-- |
-- Module: Graphics.LWGL.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Types
    ( BufferObject (..)
    , BufferTarget (..)
    , BufferUsage (..)
    , ClearBufferMask (..)
    , ComponentCount (..)
    , Location (..)
    , ShaderType (..)
    , PrimitiveType (..)
    , Program (..)
    , VertexArrayObject (..)
    , VertexAttribPointerType (..)
    , ToBitfield (..)
    , ToEnum (..)
    , ToInt (..)
    , GLbitfield
    , GLboolean
    , GLchar
    , GLenum
    , GLfloat
    , GLint
    , GLsizei
    , GLuint
    , combineBits
    , toBoolean
    ) where

import           Data.Bits   (zeroBits, (.|.))
import           Data.List   (foldl')
import           Graphics.GL (GLbitfield, GLboolean, GLchar, GLenum, GLfloat,
                              GLint, GLsizei, GLuint)
import qualified Graphics.GL as GL

-- | Class that help types to expose bitfield values.
class ToBitfield a where
    toBitfield :: a -> GLbitfield

-- | Class that help types to expose enum values.
class ToEnum a where
    toEnum :: a -> GLenum

-- | Class that help types to expose int values.
class ToInt a where
    toInt :: a -> GLint

-- | OpenGL buffer object.
newtype BufferObject = BufferObject GLuint
    deriving Show

-- | Buffer target values.
data BufferTarget
    = ArrayBuffer
    | AtomicCounterBuffer
    | CopyReadBuffer
    | DispatchIndirectBuffer
    | DrawIndirectBuffer
    | ElementArrayBuffer
    | PixelPackBuffer
    | PixelUnpackBuffer
    | QueryBuffer
    | ShaderStorageBuffer
    | TextureBuffer
    | TransformFeedbackBuffer
    | UniformBuffer
    deriving Show

instance ToEnum BufferTarget where
    toEnum ArrayBuffer             = GL.GL_ARRAY_BUFFER
    toEnum AtomicCounterBuffer     = GL.GL_ATOMIC_COUNTER_BUFFER
    toEnum CopyReadBuffer          = GL.GL_COPY_READ_BUFFER
    toEnum DispatchIndirectBuffer  = GL.GL_DISPATCH_INDIRECT_BUFFER
    toEnum DrawIndirectBuffer      = GL.GL_DRAW_INDIRECT_BUFFER
    toEnum ElementArrayBuffer      = GL.GL_ELEMENT_ARRAY_BUFFER
    toEnum PixelPackBuffer         = GL.GL_PIXEL_PACK_BUFFER
    toEnum PixelUnpackBuffer       = GL.GL_PIXEL_UNPACK_BUFFER
    toEnum QueryBuffer             = GL.GL_QUERY_BUFFER
    toEnum ShaderStorageBuffer     = GL.GL_SHADER_STORAGE_BUFFER
    toEnum TextureBuffer           = GL.GL_TEXTURE_BUFFER
    toEnum TransformFeedbackBuffer = GL.GL_TRANSFORM_FEEDBACK_BUFFER
    toEnum UniformBuffer           = GL.GL_UNIFORM_BUFFER

-- | Buffer usage values.
data BufferUsage
    = StreamDraw
    | StreamRead
    | StreamCopy
    | StaticDraw
    | StaticRead
    | StaticCopy
    | DynamicDraw
    | DynamicRead
    | DynamicCopy
    deriving Show

instance ToEnum BufferUsage where
    toEnum StreamDraw  = GL.GL_STREAM_DRAW
    toEnum StreamRead  = GL.GL_STREAM_READ
    toEnum StreamCopy  = GL.GL_STREAM_COPY
    toEnum StaticDraw  = GL.GL_STATIC_DRAW
    toEnum StaticRead  = GL.GL_STATIC_READ
    toEnum StaticCopy  = GL.GL_STATIC_COPY
    toEnum DynamicDraw = GL.GL_DYNAMIC_DRAW
    toEnum DynamicRead = GL.GL_DYNAMIC_READ
    toEnum DynamicCopy = GL.GL_DYNAMIC_COPY

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

-- | Count of components.
data ComponentCount
    = One
    | Two
    | Three
    | Four
    deriving Show

instance ToInt ComponentCount where
    toInt One   = 1
    toInt Two   = 2
    toInt Three = 3
    toInt Four  = 4

-- | Shader attribute location.
newtype Location = Location GLuint
    deriving Show

data ShaderType
    = ComputeShader
    | VertexShader
    | TessControlShader
    | TessEvaluationShader
    | GeometryShader
    | FragmentShader
    deriving Show

-- | Enumeration of shader types.
instance ToEnum ShaderType where
    toEnum ComputeShader        = GL.GL_COMPUTE_SHADER
    toEnum VertexShader         = GL.GL_VERTEX_SHADER
    toEnum TessControlShader    = GL.GL_TESS_CONTROL_SHADER
    toEnum TessEvaluationShader = GL.GL_TESS_EVALUATION_SHADER
    toEnum GeometryShader       = GL.GL_GEOMETRY_SHADER
    toEnum FragmentShader       = GL.GL_FRAGMENT_SHADER

-- | Enumeration of primitive render types.
data PrimitiveType
    = Lines
    | LinesAdjacency
    | LineLoop
    | LineStrip
    | LineStripAdjacency
    | Patches
    | Points
    | Triangles
    | TrianglesAdjacency
    | TriangleStrip
    | TriangleStripAdjacency
    deriving Show

instance ToEnum PrimitiveType where
    toEnum Lines                  = GL.GL_LINES
    toEnum LinesAdjacency         = GL.GL_LINES_ADJACENCY
    toEnum LineLoop               = GL.GL_LINE_LOOP
    toEnum LineStrip              = GL.GL_LINE_STRIP
    toEnum LineStripAdjacency     = GL.GL_LINE_STRIP_ADJACENCY
    toEnum Patches                = GL.GL_PATCHES
    toEnum Points                 = GL.GL_POINTS
    toEnum Triangles              = GL.GL_TRIANGLES
    toEnum TrianglesAdjacency     = GL.GL_TRIANGLES_ADJACENCY
    toEnum TriangleStrip          = GL.GL_TRIANGLE_STRIP
    toEnum TriangleStripAdjacency = GL.GL_TRIANGLE_STRIP_ADJACENCY

-- | Representation of a linked shader program.
newtype Program = Program GLuint
    deriving Show

-- | Vertex array object.
newtype VertexArrayObject = VertexArrayObject GLuint
    deriving Show

-- | Data types for attribute values.
data VertexAttribPointerType
    = GLByte
    | GLUnsignedByte
    | GLShort
    | GLUnsignedShort
    | GLInt
    | GLUnsignedInt
    | GLHalfFloat
    | GLFloat
    | GLDouble
    | GLFixed
    deriving Show

instance ToEnum VertexAttribPointerType where
    toEnum GLByte          = GL.GL_BYTE
    toEnum GLUnsignedByte  = GL.GL_UNSIGNED_BYTE
    toEnum GLShort         = GL.GL_SHORT
    toEnum GLUnsignedShort = GL.GL_UNSIGNED_SHORT
    toEnum GLInt           = GL.GL_INT
    toEnum GLUnsignedInt   = GL.GL_UNSIGNED_INT
    toEnum GLHalfFloat     = GL.GL_HALF_FLOAT
    toEnum GLFloat         = GL.GL_FLOAT
    toEnum GLDouble        = GL.GL_DOUBLE
    toEnum GLFixed         = GL.GL_FIXED

combineBits :: ToBitfield a => [a] -> GLbitfield
combineBits = foldl' (\a v -> a .|. toBitfield v) zeroBits

toBoolean :: Bool -> GLboolean
toBoolean False = GL.GL_FALSE
toBoolean True  = GL.GL_TRUE
