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
    , glBindTexture
    , glBindVertexArray
    , glClearColor
    , glClear
    , glBufferData
    , glBufferDataList
    , glDisableVertexAttribArray
    , glDrawArrays
    , glEnableVertexAttribArray
    , glGenBuffers
    , glGenTextures
    , glGenVertexArray
    , glTexImage2D
    , glTexParameteri
    , glUseProgram
    , glVertexAttribPointer
    ) where

import           Control.Monad       (unless)
import           Foreign             (Ptr, Storable, nullPtr, peekArray,
                                      plusPtr, sizeOf, withArray)
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

-- | Creates and initializes a buffer object's data store.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferData.xhtml>
glBufferData :: BufferTarget -> Int -> Ptr a -> BufferUsage -> IO ()
glBufferData bufferTarget size ptr bufferUsage =
    GL.glBufferData (toEnum bufferTarget) (fromIntegral size) ptr (toEnum bufferUsage)

-- | Convenice function of 'glBufferData' where data is provided in a list.
glBufferDataList :: Storable a => BufferTarget -> [a] -> BufferUsage -> IO ()
glBufferDataList bufferTarget xs bufferUsage =
    unless (null xs) $
        withArray xs $ \ptr -> do
            let first = head xs
                itemSize = sizeOf first
                storageSize = itemSize * length xs
            glBufferData bufferTarget storageSize ptr bufferUsage

-- | Disable a generic vertex attribute array.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml>
glDisableVertexAttribArray :: AttributeIndex -> IO ()
glDisableVertexAttribArray (AttributeIndex index) =
    GL.glDisableVertexAttribArray index

-- | Render primitives from array data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml>
glDrawArrays :: PrimitiveType -> Int -> Int -> IO ()
glDrawArrays primitiveType first count =
    GL.glDrawArrays (toEnum primitiveType) (fromIntegral first) (fromIntegral count)

-- | Enable a generic vertex attribute array.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml>
glEnableVertexAttribArray :: AttributeIndex -> IO ()
glEnableVertexAttribArray (AttributeIndex index) =
    GL.glEnableVertexAttribArray index

-- | Generate buffer object names
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenBuffers.xml>
glGenBuffers :: Int -> IO [BufferObject]
glGenBuffers num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenBuffers (fromIntegral num) ptr
        map BufferObject <$> peekArray num ptr

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
             width height format type_ ptr =
    GL.glTexImage2D (toEnum textureTarget) detailLevel (toInt componentCount)
                    width height 0 (toEnum format) (toEnum type_) ptr

-- | Set texture parameters.
--
-- | See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml>
glTexParameteri :: TextureTarget -> TextureParameterName
                -> TextureParameterValue -> IO ()
glTexParameteri textureTarget parameterName parameterValue =
    GL.glTexParameteri (toEnum textureTarget) (toEnum parameterName)
                       (toInt parameterValue)

-- | Installs a program object as part of current rendering state.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glUseProgram.xml>
glUseProgram :: Program -> IO ()
glUseProgram (Program program) = GL.glUseProgram program

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
