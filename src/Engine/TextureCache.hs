module Engine.TextureCache
    ( TextureCache
    , WithTextureCache
    , createTextureCache
    , CachedTexture (..)
    , getCachedTexture
    , bindCachedTexture
    )
where

import Data.IORef
import Engine.Shader
import SDL (($=))
import qualified Codec.Picture
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector.Storable
import qualified Graphics.Rendering.OpenGL as GL

type TextureCache = IORef (HashMap.HashMap Text.Text CachedTexture)

type WithTextureCache = ?textureCache :: TextureCache

createTextureCache :: IO TextureCache
createTextureCache =
    newIORef HashMap.empty

data CachedTexture = CachedTexture
    { cachedTextureGLObject :: {-# UNPACK #-} !GL.TextureObject
    , cachedTextureWidth :: {-# UNPACK #-} !Int
    , cachedTextureHeight :: {-# UNPACK #-} !Int
    }

instance Eq CachedTexture where
    ta == tb = cachedTextureGLObject ta == cachedTextureGLObject tb

instance Ord CachedTexture where
    compare ta tb =
        compare (cachedTextureGLObject ta) (cachedTextureGLObject tb)

loadTextureFromFile :: Text.Text -> IO CachedTexture
loadTextureFromFile filename = do
    dynImage <-
        Codec.Picture.readImage (Text.unpack filename)
            >>= either fail pure
    let Codec.Picture.Image iw ih idata =
            Codec.Picture.convertRGBA8 dynImage
    object <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just object
    Vector.Storable.unsafeWith idata $ \iptr -> do
        GL.texImage2D
            GL.Texture2D
            GL.NoProxy
            0
            GL.RGBA8
            (GL.TextureSize2D (fromIntegral iw) (fromIntegral ih))
            0
            (GL.PixelData GL.RGBA GL.UnsignedByte iptr)
    GL.generateMipmap' GL.Texture2D
    pure $ CachedTexture
        { cachedTextureGLObject = object
        , cachedTextureWidth = iw
        , cachedTextureHeight = ih
        }

getCachedTexture ::
    (WithTextureCache) =>
    Text.Text ->
    IO CachedTexture
getCachedTexture filename = do
    cache <- readIORef ?textureCache
    case HashMap.lookup filename cache of
        Just ctex -> pure ctex
        Nothing -> do
            ctex <- loadTextureFromFile filename
            modifyIORef' ?textureCache (HashMap.insert filename ctex)
            pure ctex

bindCachedTexture ::
    CompiledShader ->
    CachedTexture ->
    IO ()
bindCachedTexture CompiledShader {..} CachedTexture {..} = do
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just cachedTextureGLObject
    GL.uniform shaderTextureSamplerLoc
        $= GL.TextureUnit 0
    GL.uniform shaderTextureSizeLoc
        $= GL.Vector2 @Float
            (fromIntegral cachedTextureWidth)
            (fromIntegral cachedTextureHeight)
