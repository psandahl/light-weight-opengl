-- |
-- Module: Graphics.LWGL.Api
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Api
    ( glBindBuffer
    , glClearColor
    , glClear
    , glBufferData
    , glBufferDataList
    , glDisableVertexAttribArray
    , glDrawArrays
    , glEnableVertexAttribArray
    , glGenBuffers
    , glUseProgram
    , glVertexAttribPointer
    ) where

import           Control.Monad       (unless)
import           Foreign             (Ptr, Storable, nullPtr, peekArray,
                                      plusPtr, sizeOf, withArray)
import qualified Graphics.GL         as GL
import           Prelude             hiding (toEnum)

import           Graphics.LWGL.Types

-- | Bind a named buffer object
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBindBuffer.xhtml>
glBindBuffer :: BufferTarget -> BufferObject -> IO ()
glBindBuffer bufferTarget (BufferObject bufferObject) =
    GL.glBindBuffer (toEnum bufferTarget) bufferObject

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
glDisableVertexAttribArray :: Location -> IO ()
glDisableVertexAttribArray (Location location) =
    GL.glDisableVertexAttribArray location

-- | Render primitives from array data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glDrawArrays.xhtml>
glDrawArrays :: PrimitiveType -> Int -> Int -> IO ()
glDrawArrays primitiveType first count =
    GL.glDrawArrays (toEnum primitiveType) (fromIntegral first) (fromIntegral count)

-- | Enable a generic vertex attribute array.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glEnableVertexAttribArray.xhtml>
glEnableVertexAttribArray :: Location -> IO ()
glEnableVertexAttribArray (Location location) =
    GL.glEnableVertexAttribArray location

-- | Generate buffer object names
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glGenBuffers.xml>
glGenBuffers :: Int -> IO [BufferObject]
glGenBuffers num = do
    let array = replicate num 0
    withArray array $ \ptr -> do
        GL.glGenBuffers (fromIntegral num) ptr
        map BufferObject <$> peekArray num ptr

-- | Installs a program object as part of current rendering state.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glUseProgram.xml>
glUseProgram :: Program -> IO ()
glUseProgram (Program program) = GL.glUseProgram program

-- | Define an array of generic vertex attribute data.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glVertexAttribPointer.xhtml>
glVertexAttribPointer :: Location
                      -> ComponentCount
                      -> VertexAttribPointerType
                      -> Bool
                      -> Int
                      -> Int -- This is byte index into buffer
                      -> IO ()
glVertexAttribPointer (Location location) count pointerType
                      normalize stride offset = do
    let pointer = nullPtr `plusPtr` offset
    GL.glVertexAttribPointer location (toInt count) (toEnum pointerType)
                             (toBoolean normalize) (fromIntegral stride) pointer
