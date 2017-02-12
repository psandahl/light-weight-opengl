module Graphics.LWGL.Types
    ( ClearBufferMask (..)
    , GLbitfield -- Re-export
    , GLfloat -- Re-export
    , combineBits
    ) where

import           Data.Bits   (zeroBits, (.|.))
import           Data.List   (foldl')
import           Graphics.GL (GLbitfield, GLfloat)
import qualified Graphics.GL as GL

-- | Class that help types to expose bitfield values.
class ToBitfield a where
    toBitfield :: a -> GLbitfield

-- | Mask values to indicate which buffers to clear.
data ClearBufferMask
    = ColorBuffer
    | DepthBuffer
    | StencilBuffer
    deriving (Show)

instance ToBitfield ClearBufferMask where
    toBitfield ColorBuffer   = GL.GL_COLOR_BUFFER_BIT
    toBitfield DepthBuffer   = GL.GL_DEPTH_BUFFER_BIT
    toBitfield StencilBuffer = GL.GL_STENCIL_BUFFER_BIT

combineBits :: ToBitfield a => [a] -> GLbitfield
combineBits mask = foldl' (\a v -> a .|. (toBitfield v)) zeroBits mask
