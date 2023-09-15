-- This module is in charge of describing our 4D space as well as creating the image that is to be drawn by the other modules.

module Math (render) where

import Lib
import Graphics.Gloss 
import Data.List ( sortBy )
import Data.Ord
import Graphics.Gloss.Data.Vector
import Data.Maybe
  
-- | The render function is this module's sole access point. This means any 
-- changes to the outer program will require only minimal changes to this 
-- module, if any at all, and vice versa.
render :: CameraState -> Picture 
render cam = pictures (color (light blue) (tesseractPicture cam) : (if debug cam then debugPic cam else []))

-- TODO: add 'color (greyN 0.5) (viewingCube cam)' once boundLine begins
-- working properly
    
-- | Create a picture showing lines representing each of the axes and a print of
-- CameraState for debugging
debugPic :: CameraState -> [Picture]
debugPic cam = [color red line1, color blue line2, color green line3, color yellow line4, debugText]
  where
    line1 = Line [(0,0),(100*dx,100*dy)]
    (_,dx,dy,_) = direction cam
    line2 = Line [(0,0),(100*ox,100*oy)]
    (_,ox,oy,_) = orientation cam
    line3 = Line [(0,0),(100*xx,100*xy)]
    (_,xx,xy,_) = xAxis cam
    line4 = Line [(0,0),(100*rx,100*ry)]
    (_,rx,ry,_) = rotation cam
    debugText = translate (-400) (-40) . scale 0.1 0.1 . color white $ Text (show cam)

-- We begin by defining the datetype of a 4-dimensional line segment.

data Object4D = Line4D Vector4 Vector4

-- | Generate the points corresponding to a tesseract of a given size.
tesseractN :: Float -> [Vector4]
tesseractN n = [ (a,b,c,d) | a <- [n,-n], b <- [n,-n], c <- [n,-n], d <- [n,-n]]

tesseract :: [Vector4]
tesseract = tesseractN 100

tesseractRelative :: CameraState -> [Vector4]
tesseractRelative cam = map (relativeV4 cam) tesseract

-- Find the 4 closest vectors for each vector.

closests :: [Vector4] -> [[Vector4]]
closests vs = 
  [closest (vs!!n) (take n vs ++ drop (n+1) vs) | n <- [0..length vs]]
  where
    closest v vs' = 
      map ((vs'!!) . fst) 
      . take 4 . sortBy (comparing snd)
      . zip [0..] . map (distV4 v) $ vs'

-- | Draw the lines between 4 closest vectors to each vector in a list of vectors.
closestLines :: [Vector4] -> [Object4D]
closestLines vectors = concatMap (\(v,vs) -> map (Line4D v) vs) vsPair
  where
    vsPair = zip vectors (closests vectors)

tesseractRelativeLines :: CameraState -> [Object4D]
tesseractRelativeLines = closestLines . tesseractRelative

-- Proportionally reduce apparent distances as z increases:

from4Dto3D :: Vector4 -> Vector3
from4Dto3D (w,x,y,z) = (w',x',y')
  where
    (w',x',y',_) = 
      if z >= 0.00001 
        then mulSV4 (1 / (0.01*z)) (w,x,y,z)
        else mulSV4 (1 / 0.0000001) (w,x,y,z)

-- Rotate on two perpendicular planes in sequence, for turning the window around

rotate2A :: Float -> Float -> Vector3 -> Vector3
rotate2A a1 a2 (w,x,y) = (w'',x',y')
  where
    (w',x') = rotateV a1 (w,x)
    (w'',y') = rotateV a2 (w',y)

-- Take the x and y components of a 3-tuple to be projected onto the screen:

from3Dto2D :: Vector3 -> Vector
from3Dto2D (_,x,y) = (x,y)

-- Combining the previous functions, convert a given 4D vector to a 2D vector:

from4Dto2D :: Float -> Float -> Vector4 -> Vector
from4Dto2D a1 a2 = from3Dto2D . rotate2A a1 a2 . from4Dto3D

-- | Has to be applied before rotate2A to avoid having to consider viewing cube
-- rotation. bnd must be positive!
boundLine :: Float -> Vector3 -> Vector3 -> Maybe (Vector3,Vector3)
-- Scale the line based on the size of its section that is within bounds
boundLine bnd v1 v2
  | not (null (outOfBounds bnd v1)) && not (null (outOfBounds bnd v2)) = Nothing
  | not (null (outOfBounds bnd v1)) = Just (scaleBound bnd v2 v1, v2)
  | not (null (outOfBounds bnd v2)) = Just (scaleBound bnd v1 v2, v1)
  | otherwise = Just (v1,v2)

scaleBound :: Float -> Vector3 -> Vector3 -> Vector3
scaleBound bnd v1 v2 = mulSV3 mainIntersection v12 `addV3` v1
  where
    mainIntersection = minimum intersections
    intersections = map planeIntersection planes
    planes = outOfBounds bnd v2
    v12 = v2 `subV3` v1
    -- | Just a simplified version of the vplIntersectionV3 function that only
    -- requires providing a Plane.
    planeIntersection plane = 
          vplIntersectionV3 (v12,v1) (plane3ToVectors plane) (planePosition plane bnd)
    planePosition plane dist = case plane of
      WX -> (0,0,dist)
      WY -> (0,dist,0)
      XY -> (dist,0,0)
      _ -> error (shows plane " cannot be expressed as Vector3")

outOfBounds :: Float -> Vector3 -> [Plane]
outOfBounds bnd (w,x,y) 
  | abs w > (bnd + 1) = XY : outOfBounds bnd (0,x,y)
  | abs x > (bnd + 1) = WY : outOfBounds bnd (0,0,y)
  | abs y > (bnd + 1) = WX : outOfBounds bnd (0,0,0)
  | otherwise = []

-- Convert a 4D object to a 2D image on the screen:

object4DtoPicture :: Float -> Float -> Object4D -> Picture
object4DtoPicture a1 a2 (Line4D v1 v2) = Line (map (from4Dto2D a1 a2) [v1,v2])

obj4DtoPicBounded :: Float -> Float -> Float -> Object4D -> Picture
obj4DtoPicBounded bnd a1 a2 (Line4D v1 v2) = 
  Line 
  . map (from3Dto2D . rotate2A a1 a2) 
  . apply3DBounds bnd 
  . map from4Dto3D $ [v1,v2]

apply3DBounds :: Float -> [Vector3] -> [Vector3]
apply3DBounds bnd [v1,v2] = catMaybes [v1',v2']
  where
    vs' = boundLine bnd v1 v2
    v1' = fmap fst vs'
    v2' = fmap snd vs'
apply3DBounds _ _ = []

-- Finally, turn our lined tesseract into a 2D picture. We want it to only be visible
-- inside the viewing window, so we use the bounded function.

-- obj4DtoPicBounded is unfinished - replace object4DtoPicture with
-- obj4DtoPicBounded bnd for horrors beyond my understanding (at the
-- time of writing this comment)
tesseractPicture :: CameraState -> Picture
tesseractPicture cam = 
  pictures 
  . map (object4DtoPicture a1 a2) 
  . tesseractRelativeLines $ cam
  where
    (a1,a2) = (angle1 cam, angle2 cam)
    bnd = 100 -- ^ change respectively to the size of the viewing port

-- Quick and dirty way of drawing a cube: just make a tesseract with 0 thickness on the z-axis

viewingCube :: CameraState -> Picture
viewingCube cam = 
  pictures 
  . map (object4DtoPicture a1 a2) 
  . filter (\(Line4D (_,_,_,z1) (_,_,_,z2)) -> z1 >= 0 && z2 >= 0) 
  . closestLines 
  . tesseractN $ 100
  where
    (a1,a2) = (angle1 cam, angle2 cam)