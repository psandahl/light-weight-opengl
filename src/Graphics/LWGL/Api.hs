-- |
-- Module: Graphics.LWGL.Api
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Api
    ( glActiveTexture
    , glBindBuffer
    , glBindFramebuffer
    , glBindTexture
    , glBindVertexArray
    , glClearColor
    , glClear
    , glCullFace
    , glBufferData
    , glDisableVertexAttribArray
    , glDisable
    , glDrawArrays
    , glDrawBuffer
    , glDrawElements
    , glEnableVertexAttribArray
    , glEnable
    , glFramebufferTexture2D
    , glGenerateMipmap
    , glGenBuffers
    , glGenFramebuffers
    , glGenTextures
    , glGenVertexArray
    , glGetUniformLocation
    , glPolygonMode
    , glTexImage2D
    , glTexParameteri
    , glUniform1f
    , glUniform1i
    , glUniform3fv
    , glUniformMatrix4fv
    , glUseProgram
    , glViewport
    , glVertexAttribPointer
    ) where

import           Foreign             (Ptr, nullPtr, peekArray, plusPtr,
                                      withArray)
import           Foreign.C           (withCString)
import qualified Graphics.GL         as GL
import           Prelude             hiding (toEnum)

import           Graphics.LWGL.Types

-- | Select active texture unit.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glActiveTexture.xhtml>
glActiveTexture :: TextureUnit -> IO ()
glActiveTexture = GL.glActiveTexture . toEnum

-- | Bind a named buffer object
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml>
glBindBuffer :: BufferTarget -> BufferObject -> IO ()
glBindBuffer bufferTarget (BufferObject bufferObject) =
    GL.glBindBuffer (toEnum bufferTarget) bufferObject

-- | Bind a framebuffer to a framebuffer target
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindFramebuffer.xhtml>
glBindFramebuffer :: FrameBufferTarget -> FrameBuffer -> IO ()
glBindFramebuffer bufferTarget (FrameBuffer frameBuffer) =
    GL.glBindFramebuffer (toEnum bufferTarget) frameBuffer

-- | Bind a named texture to a texturing target.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindTexture.xhtml>
glBindTexture :: TextureTarget -> Texture -> IO ()
glBindTexture textureTarget (Texture texture) =
    GL.glBindTexture (toEnum textureTarget) texture

-- | Bind a vertex array object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindVertexArray.xhtml>
glBindVertexArray :: VertexArrayObject -> IO ()
glBindVertexArray (VertexArrayObject vertexArrayObject) =
    GL.glBindVertexArray vertexArrayObject

-- | Specify clear values for the color buffers.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearColor.xml>
glClearColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
glClearColor = GL.glClearColor

-- | Clear buffers to preset values.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClear.xhtml>
glClear :: [ClearBufferMask] -> IO ()
glClear = GL.glClear . combineBits

-- | Specify whether front- or back-facing facets can be culled.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glCullFace.xhtml>
glCullFace :: PolygonFace -> IO ()
glCullFace = GL.glCullFace . toEnum

-- | Creates and initializes a buffer object's data store.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml>
glBufferData :: BufferTarget -> Int -> Ptr a -> BufferUsage -> IO ()
glBufferData bufferTarget size ptr bufferUsage =
    GL.glBufferData (toEnum bufferTarget) (fromIntegral size) ptr (toEnum bufferUsage)

-- | Disable a generic vertex attribute array.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml>
glDisableVertexAttribArray :: AttributeIndex -> IO ()
glDisableVertexAttribArray (AttributeIndex index) =
    GL.glDisableVertexAttribArray index

-- | Disable server-side GL capabilities.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnable.xhtml>
glDisable :: EnableCapability -> IO ()
glDisable = GL.glDisable . toEnum

-- | Render primitives from array data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml>
glDrawArrays :: PrimitiveType -> Int -> Int -> IO ()
glDrawArrays primitiveType first count =
    GL.glDrawArrays (toEnum primitiveType) (fromIntegral first) (fromIntegral count)

-- | Specify which color buffers are to be drawn into.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawBuffer.xhtml>
glDrawBuffer :: DrawBufferMode -> IO ()
glDrawBuffer = GL.glDrawBuffer . toEnum

-- | Render primitives from array data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawElements.xhtml>
glDrawElements :: PrimitiveType -> Int -> DrawElementsType -> Ptr a -> IO ()
glDrawElements primitiveType count drawElementsType =
    GL.glDrawElements (toEnum primitiveType) (fromIntegral count) (toEnum drawElementsType)

-- | Enable a generic vertex attribute array.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml>
glEnableVertexAttribArray :: AttributeIndex -> IO ()
glEnableVertexAttribArray (AttributeIndex index) =
    GL.glEnableVertexAttribArray index

-- | Enable server-side GL capabilities.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnable.xhtml>
glEnable :: EnableCapability -> IO ()
glEnable = GL.glEnable . toEnum

-- | Attach a level of a texture object as a logical buffer of a framebuffer object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glFramebufferTexture.xhtml>
glFramebufferTexture2D :: FrameBufferTarget -> FrameBufferAttachment
                       -> TextureTarget -> Texture -> Int -> IO ()
glFramebufferTexture2D bufferTarget bufferAttachment textureTarget
                       (Texture texture) level =
    GL.glFramebufferTexture2D (toEnum bufferTarget) (toEnum bufferAttachment)
                              (toEnum textureTarget) texture (fromIntegral level)

-- | Generate mipmaps for a specified texture object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenerateMipmap.xhtml>
glGenerateMipmap :: TextureTarget -> IO ()
glGenerateMipmap = GL.glGenerateMipmap . toEnum

-- | Generate buffer object names
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenBuffers.xml>
glGenBuffers :: Int -> IO [BufferObject]
glGenBuffers num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenBuffers (fromIntegral num) ptr
        map BufferObject <$> peekArray num ptr

-- | Generate framebuffer object names.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenFramebuffers.xhtml>
glGenFramebuffers :: Int -> IO [FrameBuffer]
glGenFramebuffers num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenFramebuffers (fromIntegral num) ptr
        map FrameBuffer <$> peekArray num ptr

-- | Generate texture names.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenTextures.xhtml>
glGenTextures :: Int -> IO [Texture]
glGenTextures num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenTextures (fromIntegral num) ptr
        map Texture <$> peekArray num ptr

-- | Generate vertex array object names.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGenVertexArrays.xhtml>
glGenVertexArray :: Int -> IO [VertexArrayObject]
glGenVertexArray num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenVertexArrays (fromIntegral num) ptr
        map VertexArrayObject <$> peekArray num ptr

-- | Returns the location of a uniform variable.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetUniformLocation.xhtml>
glGetUniformLocation :: Program -> String -> IO Location
glGetUniformLocation (Program program) name =
    withCString name $ \cstring ->
        Location <$> GL.glGetUniformLocation program cstring

-- | Select a polygon rasterization mode.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glPolygonMode.xhtml>
glPolygonMode :: PolygonFace -> PolygonMode -> IO ()
glPolygonMode face = GL.glPolygonMode (toEnum face) . toEnum

-- | Specify a two-dimensional texture image.
--
-- | Note: No border parameter. Always zero.
-- | See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml>
glTexImage2D :: TextureTarget
             -> ImageDetailLevel
             -> ImageComponentCount
             -> Width
             -> Height
             -> PixelFormat
             -> PixelType
             -> Ptr a
             -> IO ()
glTexImage2D textureTarget detailLevel componentCount
             width height format type_ =
    GL.glTexImage2D (toEnum textureTarget) detailLevel (toInt componentCount)
                    width height 0 (toEnum format) (toEnum type_)

-- | Set texture parameters.
--
-- | See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml>
glTexParameteri :: TextureTarget -> TextureParameterName
                -> TextureParameterValue -> IO ()
glTexParameteri textureTarget parameterName parameterValue =
    GL.glTexParameteri (toEnum textureTarget) (toEnum parameterName)
                       (toInt parameterValue)

-- | Specify the value of a uniform variable for the current program object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml>
glUniform1f :: Location -> Float -> IO ()
glUniform1f (Location location) = GL.glUniform1f location

-- | Specify the value of a uniform variable for the current program object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml>
glUniform1i :: Location -> Int -> IO ()
glUniform1i (Location location) = GL.glUniform1i location . fromIntegral

-- | Specify the value of a uniform variable for the current program object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml>
glUniform3fv :: Location -> Int -> Ptr GLfloat -> IO ()
glUniform3fv (Location location) count =
    GL.glUniform3fv location (fromIntegral count)

-- | Specify the value of a uniform variable for the current program object.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml>
glUniformMatrix4fv :: Location -> Int -> Bool -> Ptr GLfloat -> IO ()
glUniformMatrix4fv (Location location) count transpose =
    GL.glUniformMatrix4fv location (fromIntegral count) (toBoolean transpose)

-- | Installs a program object as part of current rendering state.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glUseProgram.xml>
glUseProgram :: Program -> IO ()
glUseProgram (Program program) = GL.glUseProgram program

-- | Set the viewport
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glViewport.xhtml>
glViewport :: Int -> Int -> Int -> Int -> IO ()
glViewport x y width height =
    GL.glViewport (fromIntegral x) (fromIntegral y)
                  (fromIntegral width) (fromIntegral height)

-- | Define an array of generic vertex attribute data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribPointer.xhtml>
glVertexAttribPointer :: AttributeIndex
                      -> ComponentCount
                      -> VertexAttribPointerType
                      -> Bool
                      -> Int
                      -> Int -- This is byte index into buffer
                      -> IO ()
glVertexAttribPointer (AttributeIndex index) count pointerType
                      normalize stride offset = do
    let pointer = nullPtr `plusPtr` offset
    GL.glVertexAttribPointer index (toInt count) (toEnum pointerType)
                             (toBoolean normalize) (fromIntegral stride) pointer
