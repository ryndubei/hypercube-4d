-- Module in charge of processing and generally manipulating user input
-- with vectors, in order to get a new CameraPosition to feed to the Math
-- module.

module Input (handleKeys, update) where

import Lib 
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S
import Graphics.Gloss.Geometry.Angle

handleKeys :: Event -> CameraState -> CameraState
handleKeys (EventKey (Char 'g') Down _ _) _ = initialState
handleKeys (EventKey (Char 'b') Down _ _) cam = cam { debug = not (debug cam) } 
handleKeys (EventKey k Down _ _) cam =
  if isMovementKey k
    then cam {keys = S.insert k (keys cam)}
    else cam
handleKeys (EventKey k Up _ _) cam = cam {keys = S.delete k (keys cam)}

handleKeys _ cam = cam

-- | w/s - Z axis, a/d - X axis, q/e - W axis, r/f - Y axis. 
-- Additionally, h/l - turn on the XZ plane, j/k - turn on the YZ plane, u/i - turn on the WZ plane.
isMovementKey :: Key -> Bool
isMovementKey = flip S.member movementKeys
  where
    movementKeys :: S.Set Key
    movementKeys = 
      S.fromList 
      [Char 'w', Char 's', Char 'a', Char 'd', Char 'q', Char 'e', Char 'r', Char 'f', 
      Char 'u', Char 'i', Char 'h', Char 'j', Char 'k', Char 'l',
      SpecialKey KeyUp, SpecialKey KeyDown, SpecialKey KeyRight, SpecialKey KeyLeft]

update :: Float -> CameraState -> CameraState
update _ = handleMove

handleMove :: CameraState -> CameraState
handleMove cam = S.foldr moveKeys cam (keys cam)
  where
    moveKeys :: Key -> CameraState -> CameraState
    moveKeys (Char c) cam' =
      case c of
        -- Movement
        'w' -> cam' { location = location cam' `addV4` direction cam' }
        's' -> cam' { location = location cam' `subV4` direction cam' }
        'q' -> cam' { location = location cam' `addV4` rotation cam' }
        'e' -> cam' { location = location cam' `subV4` rotation cam' }
        'r' -> cam' { location = location cam' `addV4` orientation cam' }
        'f' -> cam' { location = location cam' `subV4` orientation cam' }
        'd' -> cam' { location = location cam' `addV4` xAxis cam' }
        'a' -> cam' { location = location cam' `subV4` xAxis cam' }
        -- Turning
        'h' -> rotateAll cam' XZ ds
        'l' -> rotateAll cam' XZ (-ds)
        'k' -> rotateAll cam' YZ ds
        'j' -> rotateAll cam' YZ (-ds)
        'u' -> rotateAll cam' WZ ds
        'i' -> rotateAll cam' WZ (-ds)
        _ -> cam'
    moveKeys (SpecialKey k) cam' =
      case k of
        -- Viewing
        KeyLeft -> cam' { angle1 = angle1 cam' + ds}
        KeyRight -> cam' { angle1 = angle1 cam' - ds}
        KeyDown -> cam' { angle2 = angle2 cam' + ds}
        KeyUp -> cam' { angle2 = angle2 cam' - ds}
        _ -> cam'
    moveKeys _ cam' = cam'
    ds = degToRad 1
    rotateAll cam' plane a = cam' 
      { direction = rotateNormalize cam' plane a (direction cam')
      , orientation = rotateNormalize cam' plane a (orientation cam')
      , rotation = rotateNormalize cam' plane a (rotation cam')}
    
rotateNormalize :: CameraState -> Plane -> Float -> Vector4 -> Vector4
rotateNormalize cam pl a v = normalizeV4 (rotateV4Relative cam pl a v)