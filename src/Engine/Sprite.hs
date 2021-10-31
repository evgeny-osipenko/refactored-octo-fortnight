module Engine.Sprite
    ( BlendMode (..)
    , Card (..)
    , Sprite (..)
    , paintSprites
    , WithQuadIndexBuffer
    , createQuadIndexBuffer
    )
where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Engine.Location
import Engine.Rect
import Engine.Shader
import Engine.TextureCache
import Engine.Util
import Engine.Viewport
import Foreign.Ptr
import Foreign.Storable
import SDL (($=))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Data.Vector.Storable.Mutable as Vector.Mutable
import qualified Data.Vector.Storable as Vector.Storable
import qualified Graphics.Rendering.OpenGL as GL

data BlendMode
    = BlendOverwrite
    | BlendAlpha
    | BlendAdd
    | BlendAddSoftSaturated
    | BlendModulate
    deriving (Eq, Ord)

data Card = Card
    { cardTextureFile :: Text.Text
    , cardTextureFilter :: (GL.MinificationFilter, GL.MagnificationFilter)
    , cardRegion :: Rect Float
    , cardShader :: ShaderName
    , cardBlendMode :: BlendMode
    }

data Sprite = Sprite
    { spriteLocation :: Location
    , spriteCard :: Card
    , spriteZOrder :: Int
    }

paintSprites ::
    ( WithShaderCache
    , WithQuadIndexBuffer
    , WithTextureCache
    , WithViewportLocationRef
    , WithWindowSizeRef
    ) =>
    Sequence.Seq Sprite ->
    IO ()
paintSprites sprites = do
    rset <- prepareSprites sprites
    paintSpriteSet rset

data SpriteRenderingMode = SpriteRenderingMode
    { srmZOrder :: Int
    , srmShader :: CompiledShader
    , srmTexture :: CachedTexture
    , srmTextureFilter :: (GL.MinificationFilter, GL.MagnificationFilter)
    , srmBlendMode :: BlendMode
    }
    deriving (Eq, Ord)

data SpriteRenderingData = SpriteRenderingData
    { srdLocation :: Location
    , srdTextureRect :: Rect Float
    }

type SpriteRenderingSet =
    Map.Map
        SpriteRenderingMode
        (Sequence.Seq SpriteRenderingData)

prepareSprites ::
    (WithShaderCache, WithTextureCache) =>
    Sequence.Seq Sprite ->
    IO SpriteRenderingSet
prepareSprites = foldM insertOne Map.empty
  where
    insertOne rset Sprite {spriteCard = Card {..}, ..} = do
        compiledShader <- getShader cardShader
        cachedTexture <- getCachedTexture cardTextureFile
        let rmode =
                SpriteRenderingMode
                    { srmZOrder = spriteZOrder
                    , srmShader = compiledShader
                    , srmTexture = cachedTexture
                    , srmTextureFilter = cardTextureFilter
                    , srmBlendMode = cardBlendMode
                    }
        let rdata =
                SpriteRenderingData
                    { srdLocation = spriteLocation
                    , srdTextureRect = cardRegion
                    }
        pure $!
            Map.insertWith
                (flip (<>))
                rmode
                (Sequence.singleton rdata) rset

buildVertexBufferData ::
    Sequence.Seq SpriteRenderingData -> IO (Vector.Storable.Vector Float)
buildVertexBufferData rdataSeq = do
    mv <- Vector.Mutable.unsafeNew (48 * Sequence.length rdataSeq)
    Vector.Mutable.unsafeWith mv $ \ptr -> do
        forSequenceM_ rdataSeq $ \i rdata -> do
            let base = 48 * i
            pokeElemOff ptr (base +  0) (-1)
            pokeElemOff ptr (base +  1) (-1)
            pokeRenderingData ptr (base +  0) rdata
            pokeElemOff ptr (base + 12)   1
            pokeElemOff ptr (base + 13) (-1)
            pokeRenderingData ptr (base + 12) rdata
            pokeElemOff ptr (base + 24)   1
            pokeElemOff ptr (base + 25)   1
            pokeRenderingData ptr (base + 24) rdata
            pokeElemOff ptr (base + 36) (-1)
            pokeElemOff ptr (base + 37)   1
            pokeRenderingData ptr (base + 36) rdata
    Vector.Storable.freeze mv
  where
    pokeRenderingData ptr offset SpriteRenderingData {..} = do
        let Location lxx lxy lxw lyx lyy lyw = srdLocation
        let Rect tax tay tbx tby = srdTextureRect
        pokeElemOff ptr (offset +  2) lxx
        pokeElemOff ptr (offset +  3) lxy
        pokeElemOff ptr (offset +  4) lxw
        pokeElemOff ptr (offset +  5) lyx
        pokeElemOff ptr (offset +  6) lyy
        pokeElemOff ptr (offset +  7) lyw
        pokeElemOff ptr (offset +  8) tax
        pokeElemOff ptr (offset +  9) tay
        pokeElemOff ptr (offset + 10) tbx
        pokeElemOff ptr (offset + 11) tby

updateMode :: (Eq a) => IORef (Maybe a) -> a -> IO () -> IO ()
updateMode prevModeRef newMode updater = do
    mbPrevMode <- readIORef prevModeRef
    case mbPrevMode of
        Just prevMode | prevMode == newMode ->
            pure ()
        _ -> do
            updater
            writeIORef prevModeRef (Just newMode)

setupBlendMode :: BlendMode -> IO ()
setupBlendMode = \case
    BlendOverwrite -> do
        GL.blend $= GL.Disabled
    BlendAlpha -> do
        GL.blend $= GL.Enabled
        GL.blendEquation $= GL.FuncAdd
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    BlendAdd -> do
        GL.blend $= GL.Enabled
        GL.blendEquation $= GL.FuncAdd
        GL.blendFunc $= (GL.One, GL.One)
    BlendAddSoftSaturated -> do
        GL.blend $= GL.Enabled
        GL.blendEquation $= GL.FuncAdd
        GL.blendFunc $= (GL.OneMinusDstColor, GL.One)
    BlendModulate -> do
        GL.blend $= GL.Enabled
        GL.blendEquation $= GL.FuncAdd
        GL.blendFunc $= (GL.DstColor, GL.Zero)

paintSpriteSet ::
    ( WithQuadIndexBuffer
    , WithViewportLocationRef
    , WithWindowSizeRef
    ) =>
    SpriteRenderingSet ->
    IO ()
paintSpriteSet rset = do
    prevShaderRef <- newIORef Nothing
    prevTextureRef <- newIORef Nothing
    prevTextureFilterRef <- newIORef Nothing
    prevBlendModeRef <- newIORef Nothing
    forMapM_ rset $ \SpriteRenderingMode {..} rdataSeq -> do
        updateMode prevShaderRef srmShader $ do
            GL.currentProgram $= Just (shaderProgram srmShader)
            setupUniformViewportLocation srmShader
            setupUniformWindowSize srmShader
        updateMode prevTextureRef srmTexture $ do
            bindCachedTexture srmShader srmTexture
        updateMode prevTextureFilterRef srmTextureFilter $ do
            GL.activeTexture $= GL.TextureUnit 0
            GL.textureFilter GL.Texture2D $=
                srmTextureFilter
        updateMode prevBlendModeRef srmBlendMode $ do
            setupBlendMode srmBlendMode
        rdataVec <- buildVertexBufferData rdataSeq
        bracket GL.genObjectName GL.deleteObjectName $ \buffer -> do
            let spriteCount = Sequence.length rdataSeq
            GL.bindBuffer GL.ArrayBuffer $= Just buffer
            Vector.Storable.unsafeWith rdataVec $ \ptr -> do
                GL.bufferData GL.ArrayBuffer $=
                    ( fromIntegral (Vector.Storable.length rdataVec * 4)
                    , ptr
                    , GL.StreamDraw
                    )
            let stride = 48
            GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
            GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
            GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
            GL.vertexAttribArray (GL.AttribLocation 3) $= GL.Enabled
            let setVaa index offset dims = do
                    GL.bindBuffer GL.ArrayBuffer $= Just buffer
                    GL.vertexAttribPointer (GL.AttribLocation index) $=
                        ( GL.ToFloat
                        , GL.VertexArrayDescriptor
                            dims
                            GL.Float
                            stride
                            (plusPtr nullPtr (offset*4))
                        )
            GL.bindBuffer GL.ElementArrayBuffer $= Just ?quadIndexBuffer
            let drawChunk offset = do
                    let chunkSize = min maxQuadCount (spriteCount - offset)
                    setVaa 0 (offset * fromIntegral stride + 0) 2
                    setVaa 1 (offset * fromIntegral stride + 2) 3
                    setVaa 2 (offset * fromIntegral stride + 5) 3
                    setVaa 3 (offset * fromIntegral stride + 8) 4
                    when (chunkSize > 0) $ do
                        GL.drawElements
                            GL.Triangles
                            (fromIntegral (chunkSize * 6))
                            GL.UnsignedShort
                            nullPtr
                        drawChunk (offset + chunkSize)
            drawChunk 0

type WithQuadIndexBuffer = ?quadIndexBuffer :: GL.BufferObject

maxQuadCount :: Int
maxQuadCount = 0x2000

createQuadIndexBuffer :: IO GL.BufferObject
createQuadIndexBuffer = do
    let dataList =
            concatMap
                ( \n ->
                    let i = fromIntegral n * 4 in
                        [i, i+1, i+2, i, i+2, i+3]
                )
                [0 .. maxQuadCount - 1]
    let dataVector = Vector.Storable.fromList @Int16 dataList
    buffer <- GL.genObjectName
    GL.bindBuffer GL.ElementArrayBuffer $= Just buffer
    Vector.Storable.unsafeWith dataVector $ \ptr -> do
        GL.bufferData GL.ElementArrayBuffer $=
            ( fromIntegral (Vector.Storable.length dataVector * 2)
            , ptr
            , GL.StaticDraw
            )
    pure buffer
