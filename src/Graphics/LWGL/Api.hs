-- |
-- Module: Graphics.LWGL.Api
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Api
    ( glClearColor
    , glClear
    ) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Graphics.GL            as GL

import           Graphics.LWGL.Types

-- | Specify clear values for the color buffers.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glClearColor.xml>
glClearColor :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glClearColor = GL.glClearColor

-- | Clear buffers to preset values.
--
-- See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glClear.xhtml>
glClear :: MonadIO m => [ClearBufferMask] -> m ()
glClear = GL.glClear . combineBits
