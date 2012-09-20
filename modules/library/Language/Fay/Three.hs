{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.Three where

import Language.Fay.FFI
import Language.Fay.Prelude

data Object3D
instance Foreign Object3D

data Geometry
instance Foreign Geometry

data Material
instance Foreign Material

data Renderer
instance Foreign Renderer

requestAnimationFrame :: Fay () -> Fay ()
requestAnimationFrame = ffi "window.requestAnimationFrame(%1)"

setX :: Object3D -> Double -> Fay ()
setX = ffi "%1.position.x = %2"

setZ :: Object3D -> Double -> Fay ()
setZ = ffi "%1.position.z = %2"

incRotationX :: Object3D -> Double -> Fay ()
incRotationX = ffi "%1.rotation.x += %2"

incRotationY :: Object3D -> Double -> Fay ()
incRotationY = ffi "%1.rotation.y += %2"

mkPerspectiveCamera :: Double -> Double -> Double -> Double -> Fay Object3D
mkPerspectiveCamera = ffi "new THREE.PerspectiveCamera(%1,%2,%3,%4)"

mkScene :: Fay Object3D
mkScene = ffi "new THREE.Scene()"

mkCanvasRenderer :: Fay Renderer
mkCanvasRenderer = ffi "new THREE.CanvasRenderer()"

setRendererSize :: Renderer -> Double -> Double -> Fay ()
setRendererSize = ffi "%1.setSize(%2,%3)"

render :: Renderer -> Object3D -> Object3D -> Fay ()
render = ffi "%1.render(%2,%3)"

addObject3D :: Object3D -> Object3D -> Fay ()
addObject3D = ffi "%1.add(%2)"

mkCubeGeometry :: Double -> Double -> Double -> Fay Geometry
mkCubeGeometry = ffi "new THREE.CubeGeometry(%1,%2,%3)"

mkMeshBasicMaterial :: Double -> Bool -> Fay Material
mkMeshBasicMaterial = ffi "new THREE.MeshBasicMaterial({color:%1, wireframe: %2})"

mkMesh :: Geometry -> Material -> Fay Object3D
mkMesh = ffi "new THREE.Mesh(%1,%2)"
