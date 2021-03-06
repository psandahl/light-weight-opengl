-- |
-- Module: Graphics.LWGL.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Types
    ( AttributeIndex (..)
    , BufferObject (..)
    , BufferTarget (..)
    , BufferUsage (..)
    , ClearBufferMask (..)
    , ComponentCount (..)
    , DrawBufferMode (..)
    , DrawElementsType (..)
    , EnableCapability (..)
    , FrameBuffer (..)
    , FrameBufferAttachment (..)
    , FrameBufferTarget (..)
    , Height
    , ImageDetailLevel
    , ImageComponentCount (..)
    , Location (..)
    , ShaderType (..)
    , PixelFormat (..)
    , PixelType (..)
    , PolygonFace (..)
    , PolygonMode (..)
    , PrimitiveType (..)
    , Program (..)
    , Texture (..)
    , TextureParameterName (..)
    , TextureParameterValue (..)
    , TextureTarget (..)
    , TextureUnit (..)
    , VertexArrayObject (..)
    , VertexAttribPointerType (..)
    , Width
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

-- | Attribute index.
newtype AttributeIndex = AttributeIndex GLuint
    deriving Show

-- | OpenGL buffer object.
newtype BufferObject = BufferObject GLuint
    deriving Show

-- | Buffer target values for binding.
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

data DrawBufferMode
    = DrawNone
    deriving Show

instance ToEnum DrawBufferMode where
    toEnum DrawNone = GL.GL_NONE

-- | Enumeration of index types at drawElements.
data DrawElementsType
    = IdxUnsignedByte
    | IdxUnsignedShort
    | IdxUnsignedInt
    deriving Show

instance ToEnum DrawElementsType where
    toEnum IdxUnsignedByte  = GL.GL_UNSIGNED_BYTE
    toEnum IdxUnsignedShort = GL.GL_UNSIGNED_SHORT
    toEnum IdxUnsignedInt   = GL.GL_UNSIGNED_INT

-- | Enable/disable capabilities.
data EnableCapability
    = Blend
    | DepthTest
    | CullFace
    | MultiSample
    | ProgramPointSize
    deriving Show

instance ToEnum EnableCapability where
    toEnum Blend            = GL.GL_BLEND
    toEnum DepthTest        = GL.GL_DEPTH_TEST
    toEnum CullFace         = GL.GL_CULL_FACE
    toEnum MultiSample      = GL.GL_MULTISAMPLE
    toEnum ProgramPointSize = GL.GL_PROGRAM_POINT_SIZE

-- | Representation of a FrameBuffer.
newtype FrameBuffer = FrameBuffer GLuint
    deriving Show

-- | Representation of framebuffer target.
data FrameBufferTarget
    = GLFrameBuffer
    | GLReadFrameBuffer
    | GLDrawFrameBuffer
    deriving Show

instance ToEnum FrameBufferTarget where
    toEnum GLFrameBuffer     = GL.GL_FRAMEBUFFER
    toEnum GLReadFrameBuffer = GL.GL_READ_FRAMEBUFFER
    toEnum GLDrawFrameBuffer = GL.GL_DRAW_FRAMEBUFFER

-- | Representation of framebuffer attachement.
data FrameBufferAttachment
    = GLDepthAttachment
    deriving Show

instance ToEnum FrameBufferAttachment where
    toEnum GLDepthAttachment = GL.GL_DEPTH_ATTACHMENT

-- | Image height.
type Height = GLsizei

-- | Detail level of image.
type ImageDetailLevel = GLint

-- | Count of image components.
data ImageComponentCount
    = ImgDepthComponent
    | ImgDepthStencil
    | ImgRed
    | ImgRG
    | ImgRGB
    | ImgRGBA
    deriving Show

instance ToInt ImageComponentCount where
    toInt ImgDepthComponent = fromIntegral GL.GL_DEPTH_COMPONENT
    toInt ImgDepthStencil   = fromIntegral GL.GL_DEPTH_STENCIL
    toInt ImgRed            = fromIntegral GL.GL_RED
    toInt ImgRG             = fromIntegral GL.GL_RG
    toInt ImgRGB            = fromIntegral GL.GL_RGB
    toInt ImgRGBA           = fromIntegral GL.GL_RGBA

-- | Shader attribute location.
newtype Location = Location GLint
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

-- | Pixel format enumeration.
data PixelFormat
    = PxlRed
    | PxlRG
    | PxlRGB
    | PxlBGR
    | PxlRGBA
    | PxlBGRA
    | PxlDepthComponent
    deriving Show

instance ToEnum PixelFormat where
    toEnum PxlRed            = GL.GL_RED
    toEnum PxlRG             = GL.GL_RG
    toEnum PxlRGB            = GL.GL_RGB
    toEnum PxlBGR            = GL.GL_BGR
    toEnum PxlRGBA           = GL.GL_RGBA
    toEnum PxlBGRA           = GL.GL_BGRA
    toEnum PxlDepthComponent = GL.GL_DEPTH_COMPONENT

-- | Pixel type enumeration.
data PixelType
    = PxlUnsignedByte
    | PxlByte
    | PxlUnsignedShort
    | PxlShort
    | PxlFloat
    deriving Show

instance ToEnum PixelType where
    toEnum PxlUnsignedByte  = GL.GL_UNSIGNED_BYTE
    toEnum PxlByte          = GL.GL_BYTE
    toEnum PxlUnsignedShort = GL.GL_UNSIGNED_SHORT
    toEnum PxlShort         = GL.GL_SHORT
    toEnum PxlFloat         = GL.GL_FLOAT

data PolygonFace
    = Front
    | Back
    | FrontAndBack
    deriving Show

instance ToEnum PolygonFace where
    toEnum Front        = GL.GL_FRONT
    toEnum Back         = GL.GL_BACK
    toEnum FrontAndBack = GL.GL_FRONT_AND_BACK

data PolygonMode
    = Fill
    | Line
    | Point
    deriving Show

instance ToEnum PolygonMode where
    toEnum Fill  = GL.GL_FILL
    toEnum Line  = GL.GL_LINE
    toEnum Point = GL.GL_POINT

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

data ReadBufferMode
    = ReadNone
    deriving Show

-- | Representation of a linked shader program.
newtype Program = Program GLuint
    deriving Show

-- | Representation of a texture.
newtype Texture = Texture GLuint
    deriving Show

-- | Enumeration of texture parameter names.
data TextureParameterName
    = TextureWrapS
    | TextureWrapT
    | TextureMagFilter
    | TextureMinFilter
    deriving Show

instance ToEnum TextureParameterName where
    toEnum TextureWrapS     = GL.GL_TEXTURE_WRAP_S
    toEnum TextureWrapT     = GL.GL_TEXTURE_WRAP_T
    toEnum TextureMagFilter = GL.GL_TEXTURE_MAG_FILTER
    toEnum TextureMinFilter = GL.GL_TEXTURE_MIN_FILTER

data TextureParameterValue
    = GLLinear
    | GLNearest
    | GLRepeat
    | GLLinearMipmapLinear
    | GLClampToEdge
    deriving Show

instance ToInt TextureParameterValue where
    toInt GLLinear             = fromIntegral GL.GL_LINEAR
    toInt GLNearest            = fromIntegral GL.GL_LINEAR
    toInt GLRepeat             = fromIntegral GL.GL_REPEAT
    toInt GLLinearMipmapLinear = fromIntegral GL.GL_LINEAR_MIPMAP_LINEAR
    toInt GLClampToEdge        = fromIntegral GL.GL_CLAMP_TO_EDGE

-- | Representation of a texture unit.
newtype TextureUnit = TextureUnit GLuint
    deriving Show

instance ToEnum TextureUnit where
    toEnum (TextureUnit unit) = GL.GL_TEXTURE0 + unit

-- | Enumeration of texture targets.
data TextureTarget
    = Texture1D
    | Texture2D
    | Texture3D
    | Texture1DArray
    | Texture2DArray
    | TextureRectangle
    | TextureCubeMap
    | TextureCubeMapArray
    | Texture2DMultisample
    | Texture2DMultisampleArray
    deriving Show

instance ToEnum TextureTarget where
    toEnum Texture1D                 = GL.GL_TEXTURE_1D
    toEnum Texture2D                 = GL.GL_TEXTURE_2D
    toEnum Texture3D                 = GL.GL_TEXTURE_3D
    toEnum Texture1DArray            = GL.GL_TEXTURE_1D_ARRAY
    toEnum Texture2DArray            = GL.GL_TEXTURE_2D_ARRAY
    toEnum TextureRectangle          = GL.GL_TEXTURE_RECTANGLE
    toEnum TextureCubeMap            = GL.GL_TEXTURE_CUBE_MAP
    toEnum TextureCubeMapArray       = GL.GL_TEXTURE_CUBE_MAP_ARRAY
    toEnum Texture2DMultisample      = GL.GL_TEXTURE_2D_MULTISAMPLE
    toEnum Texture2DMultisampleArray = GL.GL_TEXTURE_2D_MULTISAMPLE_ARRAY

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

-- | Image width.
type Width = GLsizei

combineBits :: ToBitfield a => [a] -> GLbitfield
combineBits = foldl' (\a v -> a .|. toBitfield v) zeroBits

toBoolean :: Bool -> GLboolean
toBoolean False = GL.GL_FALSE
toBoolean True  = GL.GL_TRUE
