-- |
-- Module: Graphics.LWGL.Shader
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: BSD3
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.LWGL.Shader
    ( loadShaders
    ) where

import           Control.Exception     (SomeException, try)
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Char8 as BS
import           Foreign               (Ptr, nullPtr, peek, with)
import           Foreign.C             (peekCString, withCString)
import qualified Graphics.GL           as GL
import           Prelude               hiding (toEnum)

import           Graphics.LWGL.Types

loadShaders :: [(ShaderType, FilePath)] -> IO (Either String Program)
loadShaders = undefined

-- | Compile a single shader.
compileShader :: (ShaderType, FilePath) -> IO (Either String GLuint)
compileShader (shaderType, filePath) = do
    eFile <- tryReadFile filePath
    case eFile of
        Right src -> do
            handle <- GL.glCreateShader $ toEnum shaderType
            setShaderSource handle src
            GL.glCompileShader handle
            status <- getShaderStatus handle GL.GL_COMPILE_STATUS
            if status == GL.GL_TRUE
                then return $ Right handle
                else do
                    errLog <- getInfoLog handle GL.glGetShaderInfoLog
                    return $ Left (filePath ++ ": " ++ errLog)

        Left e -> return $ Left (show e)

-- | Link shaders to a program.
linkShaders :: [GLuint] -> IO (Either String Program)
linkShaders shaders = do
    handle <- GL.glCreateProgram
    mapM_ (GL.glAttachShader handle) shaders
    GL.glLinkProgram handle
    status <- getShaderStatus handle GL.GL_LINK_STATUS
    if status == GL.GL_TRUE
        then do
            mapM_ (GL.glDetachShader handle) shaders
            mapM_ GL.glDeleteShader shaders
            return $ Right (Program handle)
        else do
            errLog <- getInfoLog handle GL.glGetProgramInfoLog
            mapM_ GL.glDeleteShader shaders
            GL.glDeleteProgram handle
            return $ Left errLog

setShaderSource :: GLuint -> ByteString -> IO ()
setShaderSource handle src = undefined
    BS.useAsCString src $ \cstring ->
        with cstring $ \ptr ->
            GL.glShaderSource handle 1 ptr nullPtr

getShaderStatus :: GLuint -> GLenum -> IO GLboolean
getShaderStatus handle shaderType =
    with 0 $ \ptr -> do
        GL.glGetShaderiv handle shaderType ptr
        v <- peek ptr
        if v == 0
            then return GL.GL_FALSE
            else return GL.GL_TRUE

getInfoLog :: GLuint
           -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
           -> IO String
getInfoLog handle getter = do
    let str = Prelude.replicate 500 '\0'
    withCString str $ \ptr -> do
        getter handle 500 nullPtr ptr
        peekCString ptr

tryReadFile :: FilePath -> IO (Either SomeException ByteString)
tryReadFile = try . BS.readFile
