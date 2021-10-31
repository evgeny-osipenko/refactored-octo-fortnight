module Engine.Shader
    ( ShaderCache
    , WithShaderCache
    , ShaderName (..)
    , createShaderCache
    , getShader
    , CompiledShader (..)
    )
where

import Control.Monad
import Engine.Util
import SDL (get, ($=))
import qualified Data.ByteString as ByteString
import qualified Graphics.Rendering.OpenGL as GL

newtype ShaderCache = ShaderCache
    { cachedMainShader :: CompiledShader
    }

type WithShaderCache = ?shaderCache :: ShaderCache

data ShaderName = MainShader

createShaderCache :: IO ShaderCache
createShaderCache =
    ShaderCache
        <$> makeMainShader

getShader :: (WithShaderCache) => ShaderName -> IO CompiledShader
getShader = \case
    MainShader -> pure $ cachedMainShader ?shaderCache

data CompiledShader = CompiledShader
    { shaderProgram :: GL.Program
    , shaderWindowSizeLoc :: GL.UniformLocation
    , shaderViewportXRowLoc :: GL.UniformLocation
    , shaderViewportYRowLoc :: GL.UniformLocation
    , shaderTextureSizeLoc :: GL.UniformLocation
    , shaderTextureSamplerLoc :: GL.UniformLocation
    }

instance Eq CompiledShader where
    ta == tb = shaderProgram ta == shaderProgram tb

instance Ord CompiledShader where
    compare ta tb =
        compare (shaderProgram ta) (shaderProgram tb)

makeMainShader :: IO CompiledShader
makeMainShader = do
    vertexShader <-
        buildShader GL.VertexShader [fromFile|shaders\main_vertex.c|]
    fragmentShader <-
        buildShader GL.FragmentShader [fromFile|shaders\main_frag.c|]
    program <- buildProgram
        [vertexShader, fragmentShader]
        [ ("aArg", 0)
        , ("aPosX", 1)
        , ("aPosY", 2)
        , ("aTexRect", 3)
        ]
    CompiledShader
        <$> pure program
        <*> get (GL.uniformLocation program "uWindowSize")
        <*> get (GL.uniformLocation program "uViewX")
        <*> get (GL.uniformLocation program "uViewY")
        <*> get (GL.uniformLocation program "uTextureSize")
        <*> get (GL.uniformLocation program "uTextureSampler")

buildShader ::
    GL.ShaderType -> ByteString.ByteString -> IO GL.Shader
buildShader shaderType source = do
    shader <- GL.createShader shaderType
    GL.shaderSourceBS shader $= source
    GL.compileShader shader
    compileSuccessful <- get (GL.compileStatus shader)
    unless compileSuccessful $ do
        putStrLn $ "Failed to compile " <> show shaderType
        get (GL.shaderInfoLog shader) >>= putStrLn
        fail "buildShader failed"
    pure shader

buildProgram ::
    [GL.Shader] -> [(String, GL.GLuint)] -> IO GL.Program
buildProgram shaders boundAttribs = do
    program <- GL.createProgram
    mapM_ (GL.attachShader program) shaders
    forM_ boundAttribs $ \(name, index) -> do
        GL.attribLocation program name $= GL.AttribLocation index
    GL.linkProgram program
    linkSuccessful <- get (GL.linkStatus program)
    validationSuccessful <-
        if linkSuccessful
            then GL.validateProgram program >> get (GL.validateStatus program)
            else pure False
    unless validationSuccessful $ do
        putStrLn $ "Failed to link a shader program"
        get (GL.programInfoLog program) >>= putStrLn
        fail "buildProgram failed"
    pure program
