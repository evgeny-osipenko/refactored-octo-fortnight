module Main
    ( main
    )
where

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Data.Word
import Engine.Location
import Engine.Rect
import Engine.Shader
import Engine.Sprite
import Engine.TextureCache
import Engine.Viewport
import SDL (($=))
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear
import qualified Linear.Affine
import qualified SDL

type WithPrevTimeRef = ?prevTimeRef :: IORef Word32

type WithFrameCounter = ?frameCounter :: IORef Int

type WithFramerateMeasure = ?framerateMeasure :: IORef Word32

type WithSdlWindow = ?sdlWindow :: SDL.Window

type WithGLContext = ?glContext :: SDL.GLContext

type WithTerminateRef = ?terminateRef :: IORef Bool

type WithWorldRef = ?worldRef :: IORef World

main :: IO ()
main = do
    bracket_ SDL.initializeAll SDL.quit $ do
        let oglConfig =
                SDL.OpenGLConfig
                    { glColorPrecision = Linear.V4 8 8 8 0
                    , glDepthPrecision = 24
                    , glStencilPrecision = 8
                    , glMultisampleSamples = 1
                    , glProfile = SDL.Core SDL.Debug 3 1
                    }
        let initialWindowSize = Linear.V2 800 600
        let windowConfig =
                SDL.WindowConfig
                    { windowBorder = True
                    , windowHighDPI = False
                    , windowInputGrabbed = False
                    , windowMode = SDL.Windowed
                    , windowGraphicsContext = SDL.OpenGLContext oglConfig
                    , windowPosition = SDL.Wherever
                    , windowResizable = True
                    , windowInitialSize = fromIntegral <$> initialWindowSize
                    , windowVisible = True
                    }
        appWindow <- SDL.createWindow "gaem" windowConfig
        appGLContext <- SDL.glCreateContext appWindow
        SDL.glMakeCurrent appWindow appGLContext
        GL.debugOutput $= GL.Enabled
        GL.debugMessageCallback $= Just (\dm -> print dm)
        appQuadIndexBuffer <- createQuadIndexBuffer
        appShaderCache <- createShaderCache
        appTextureCache <- createTextureCache
        appTerminateRef <- newIORef False
        appWindowSizeRef <- newIORef initialWindowSize
        appViewportLocationRef <- newIORef $ locScale 4
        appFrameCounter <- newIORef 0
        appFramerateMeasure <- newIORef 0
        appPrevTimeRef <- newIORef =<< SDL.ticks
        appWorldRef <- newIORef initialWorld
        let ?frameCounter = appFrameCounter
        let ?framerateMeasure = appFramerateMeasure
        let ?glContext = appGLContext
        let ?prevTimeRef = appPrevTimeRef
        let ?quadIndexBuffer = appQuadIndexBuffer
        let ?sdlWindow = appWindow
        let ?shaderCache = appShaderCache
        let ?terminateRef = appTerminateRef
        let ?textureCache = appTextureCache
        let ?viewportLocationRef = appViewportLocationRef
        let ?windowSizeRef = appWindowSizeRef
        let ?worldRef = appWorldRef
        mainLoop
        SDL.glDeleteContext appGLContext
        SDL.destroyWindow appWindow

mainLoop ::
    ( WithFrameCounter
    , WithFramerateMeasure
    , WithGLContext
    , WithPrevTimeRef
    , WithQuadIndexBuffer
    , WithSdlWindow
    , WithShaderCache
    , WithTerminateRef
    , WithTextureCache
    , WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    IO ()
mainLoop = do
    events <- SDL.pollEvents
    mapM_ handleInput events
    SDL.glMakeCurrent ?sdlWindow ?glContext
    paint
    SDL.glSwapWindow ?sdlWindow
    prevTime <- readIORef ?prevTimeRef
    currentTime <- SDL.ticks
    oldWorld <- readIORef ?worldRef
    newWorld <-
        advanceWorld
            (fromIntegral (currentTime - prevTime) * 0.001)
            oldWorld
    writeIORef ?worldRef $! newWorld
    writeIORef ?prevTimeRef currentTime
    nextMeasureTime <- readIORef ?framerateMeasure
    when (currentTime > nextMeasureTime) $ do
        framerate <- readIORef ?frameCounter
        modifyIORef' ?framerateMeasure (+ 1000)
        writeIORef ?frameCounter 0
        SDL.windowTitle ?sdlWindow $=
            "gaem ("
                <> Text.pack (show $ Sequence.length newWorld)
                <> " sprites, "
                <> Text.pack (show framerate) <> " FPS)"
    terminateFlag <- readIORef ?terminateRef
    unless terminateFlag mainLoop

handleInput ::
    ( WithGLContext
    , WithSdlWindow
    , WithTerminateRef
    , WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    SDL.Event ->
    IO ()
handleInput event = do
    -- print (SDL.eventPayload event)
    case SDL.eventPayload event of
        SDL.QuitEvent -> writeIORef ?terminateRef True
        SDL.KeyboardEvent keyboardEvent ->
            handleKeyboardEvent keyboardEvent
        SDL.MouseButtonEvent mouseButtonEvent ->
            handleMouseButtonEvent mouseButtonEvent
        SDL.MouseMotionEvent mouseMotionEvent ->
            handleMouseMotionEvent mouseMotionEvent
        SDL.WindowSizeChangedEvent sizeEvent ->
            handleSizeChangedEvent sizeEvent
        SDL.MouseWheelEvent mwheelEvent -> do
            oldVloc <- readIORef ?viewportLocationRef
            mousePos <- SDL.getAbsoluteMouseLocation
            wpos <-
                convertMousePosFromClientToWorld (fromIntegral <$> mousePos)
            let k = case SDL.mouseWheelEventPos mwheelEvent of
                    Linear.V2 0 1 -> 2
                    Linear.V2 0 (-1) -> 0.5
                    _ -> 1
            let newVloc =
                    oldVloc
                    <> locTranslate wpos
                    <> locScale k
                    <> locTranslate (negate <$> wpos)
            writeIORef ?viewportLocationRef newVloc
        _ -> pure ()

handleKeyboardEvent ::
    (WithTerminateRef) =>
    SDL.KeyboardEventData ->
    IO ()
handleKeyboardEvent keyboardEvent = do
    case (keyCode, keyMotion) of
        (SDL.KeycodeQ, SDL.Pressed) ->
            writeIORef ?terminateRef True
        _ -> pure ()
  where
    keyMotion = SDL.keyboardEventKeyMotion keyboardEvent
    keyCode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)

handleMouseButtonEvent ::
    ( WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    SDL.MouseButtonEventData ->
    IO ()
handleMouseButtonEvent SDL.MouseButtonEventData {..} = do
    case (mouseButtonEventButton, mouseButtonEventMotion) of
        (SDL.ButtonLeft, SDL.Pressed) -> do
            spawnFly mouseButtonEventPos
        _ -> pure ()

handleMouseMotionEvent ::
    ( WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    SDL.MouseMotionEventData ->
    IO ()
handleMouseMotionEvent SDL.MouseMotionEventData {..} = do
    case mouseMotionEventState of
        [SDL.ButtonLeft] -> spawnFly mouseMotionEventPos
        [SDL.ButtonRight] -> do
            oldVloc <- readIORef ?viewportLocationRef
            let Linear.V2 rx ry = fromIntegral <$> mouseMotionEventRelMotion
            let newVloc =
                    locTranslate (Linear.V2 rx (-ry))
                    <> oldVloc
            writeIORef ?viewportLocationRef newVloc
        _ -> pure ()

spawnFly ::
    ( WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    Linear.Affine.Point Linear.V2 Int32 ->
    IO ()
spawnFly mpos = do
    let card = Card
            "asset/pix.png"
            ((GL.Linear', Just GL.Linear'), GL.Linear')
            (Rect 0 0 128 128)
            MainShader
            BlendAddSoftSaturated
    wpos@(Linear.V2 wx wy) <-
        convertMousePosFromClientToWorld mpos
    let newState = Linear.V3 5 wx wy
    let newSprite = Sprite
            (locTranslate wpos <> locScale 0.001)
            card
            0
    modifyIORef' ?worldRef (Sequence.|> (newState, newSprite))

convertMousePosFromClientToWorld ::
    ( WithViewportLocationRef
    , WithWindowSizeRef
    ) =>
    Linear.Affine.Point Linear.V2 Int32 ->
    IO (Linear.V2 Float)
convertMousePosFromClientToWorld (Linear.Affine.P mousePos) = do
    let Linear.V2 mx my = fmap fromIntegral mousePos
    Linear.V2 sx sy <- fmap fromIntegral <$> readIORef ?windowSizeRef
    let rmx = mx - sx / 2
    let rmy = - my + sy / 2
    viewLocation <- readIORef ?viewportLocationRef
    let Location ixx ixy ixw iyx iyy iyw = inverseLocation viewLocation
    pure $
        Linear.V2
            (ixx * rmx + ixy * rmy + ixw)
            (iyx * rmx + iyy * rmy + iyw)

handleSizeChangedEvent ::
    ( WithGLContext
    , WithSdlWindow
    , WithWindowSizeRef
    ) =>
    SDL.WindowSizeChangedEventData ->
    IO ()
handleSizeChangedEvent sizeEvent = do
    when (SDL.windowSizeChangedEventWindow sizeEvent == ?sdlWindow) $ do
        let newSize@(Linear.V2 sw sh) =
                SDL.windowSizeChangedEventSize sizeEvent
        writeIORef ?windowSizeRef (fromIntegral <$> newSize)
        SDL.glMakeCurrent ?sdlWindow ?glContext
        GL.viewport $=
            (GL.Position 0 0, GL.Size (fromIntegral sw) (fromIntegral sh))

paint ::
    ( WithFrameCounter
    , WithQuadIndexBuffer
    , WithShaderCache
    , WithTextureCache
    , WithViewportLocationRef
    , WithWindowSizeRef
    , WithWorldRef
    ) =>
    IO ()
paint = do
    GL.clearColor $= GL.Color4 0 0 0.1 1
    GL.clear [GL.ColorBuffer]
    world <- readIORef ?worldRef
    paintSprites $ snd <$> world
    GL.finish
    modifyIORef' ?frameCounter (+ 1)

type World = Sequence.Seq (Linear.V3 Float, Sprite)

initialWorld :: World
initialWorld = Sequence.empty

advanceWorld :: Float -> World -> IO World
advanceWorld unscaledDt world = do
    pure $ fmap advanceOne world
  where
    advanceOne (Linear.V3 x y z, sprite) = do
        let dx = sigma * (y - x)
        let dy = x * (rho - z) - y
        let dz = x * y - beta * z
        let newState = Linear.V3
                (x + dt * dx)
                (y + dt * dy)
                (z + dt * dz)
        let newSprite = sprite
                { spriteLocation =
                    locTranslate (Linear.V2 y z)
                        <> locScale (1 / (100 + x))
                }
        (newState, newSprite)
    dt = unscaledDt * 0.1
    rho = 28
    sigma = 10
    beta = 8/3

