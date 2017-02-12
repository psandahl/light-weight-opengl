module GLUtil.ShaderCompiler
    ( ShaderType (..)
    , ShaderRequest
    , Program (..)
    , compileShaders
    ) where

import           Control.Exception  (SomeException, try)
import           Foreign
import           Foreign.C
import           Graphics.GL.Core33

data ShaderType = FragmentShader | VertexShader
    deriving (Eq, Show)

type ShaderRequest = (ShaderType, FilePath)

newtype Program = Program GLuint

compileShaders :: [ShaderRequest] -> IO (Either String Program)
compileShaders xs = do
    eShaders <- sequence <$> mapM compileShader xs
    case eShaders of
        Right shaders -> linkShader shaders
        Left err      -> return $ Left err

compileShader :: ShaderRequest -> IO (Either String GLuint)
compileShader (shader, path) = do
    eFile <- tryReadFile path
    case eFile of
        Right src -> do
            handle <- glCreateShader $ toShaderEnum shader
            shaderSource handle src
            glCompileShader handle
            status <- compileStatus handle
            if status == GL_TRUE
                then return $ Right handle
                else do
                    errLog <- getShaderInfoLog handle
                    return $ Left (path ++ ": " ++ errLog)

        Left e -> return $ Left (show e)

linkShader :: [GLuint] -> IO (Either String Program)
linkShader shaders = do
    handle <- glCreateProgram
    mapM_ (glAttachShader handle) shaders
    glLinkProgram handle
    status <- linkStatus handle
    if status == GL_TRUE
        then do
            mapM_ (glDetachShader handle) shaders
            mapM_ glDeleteShader shaders
            return $ Right (Program handle)
        else do
            errLog <- getProgramInfoLog handle
            mapM_ glDeleteShader shaders
            glDeleteProgram handle
            return $ Left errLog

toShaderEnum :: ShaderType -> GLenum
toShaderEnum FragmentShader = GL_FRAGMENT_SHADER
toShaderEnum VertexShader   = GL_VERTEX_SHADER

tryReadFile :: FilePath -> IO (Either SomeException String)
tryReadFile = try . readFile

shaderSource :: GLuint -> String -> IO ()
shaderSource handle src =
    withCString src $ \cstring ->
        withArray [cstring] $ \ptr ->
            glShaderSource handle 1 ptr nullPtr

compileStatus :: GLuint -> IO GLboolean
compileStatus handle =
    with 0 $ \ptr -> do
        glGetShaderiv handle GL_COMPILE_STATUS ptr
        v <- peek ptr
        if v == 0
            then return GL_FALSE
            else return GL_TRUE

getShaderInfoLog :: GLuint -> IO String
getShaderInfoLog handle = do
    let str = replicate 500 '\0'
    withCString str $ \cstring -> do
        glGetShaderInfoLog handle 500 nullPtr cstring
        peekCString cstring

linkStatus :: GLuint -> IO GLboolean
linkStatus handle =
    with 0 $ \ptr -> do
        glGetProgramiv handle GL_LINK_STATUS ptr
        v <- peek ptr
        if v == 0
            then return GL_FALSE
            else return GL_TRUE

getProgramInfoLog :: GLuint -> IO String
getProgramInfoLog handle = do
    let str = replicate 500 '\0'
    withCString str $ \cstring -> do
        glGetProgramInfoLog handle 500 nullPtr cstring
        peekCString cstring
