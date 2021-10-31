module Engine.Viewport where

import Data.IORef
import Engine.Location
import Engine.Shader
import SDL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear

type WithViewportLocationRef = ?viewportLocationRef :: IORef Location

type WithWindowSizeRef = ?windowSizeRef :: IORef (Linear.V2 Int)

setupUniformViewportLocation ::
    (WithViewportLocationRef) =>
    CompiledShader ->
    IO ()
setupUniformViewportLocation CompiledShader {..} = do
    Location vxx vxy vxw vyx vyy vyw <-
        readIORef ?viewportLocationRef
    GL.uniform shaderViewportXRowLoc
        $= GL.Vector3 vxx vxy vxw
    GL.uniform shaderViewportYRowLoc
        $= GL.Vector3 vyx vyy vyw

setupUniformWindowSize ::
    (WithWindowSizeRef) =>
    CompiledShader ->
    IO ()
setupUniformWindowSize CompiledShader {..} = do
    Linear.V2 sx sy <- readIORef ?windowSizeRef
    GL.currentProgram $= Just shaderProgram
    GL.uniform shaderWindowSizeLoc
        $= GL.Vector2 @Float (fromIntegral sx) (fromIntegral sy)

