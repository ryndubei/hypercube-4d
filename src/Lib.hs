-- Module providing shared definitions for all other modules.
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib
    ( CameraState (..),
    initialState,
    Vector4,
    Vector3,
    rotateV4Relative,
    relativeV4,
    Plane (..),
    upV4,
    inV4,
    addV4,
    subV4,
    xAxis,
    normalizeV4,
    projV4,
    mulSV4,
    negV4,
    magV4,
    dotV4,
    distV4,
    mulSV3,
    addV3,
    negV3,
    subV3,
    componentsV3,
    vplIntersectionV3,
    plane3ToVectors,
    ) where

import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
    ( detLaplace,
      fromList,
      fromLists,
      multStd,
      rref,
      toList,
      Matrix,
      (<|>),
      submatrix, getElem,
      )
import Data.List ( transpose )

import qualified Data.Set as S
import Graphics.Gloss.Geometry.Angle

data CameraState = Position
  { location :: Vector4    -- ^ The location of the camera in space
  , direction :: Vector4   -- ^ Unit vector representing the direction of the camera.
  , orientation :: Vector4 -- ^ Unit vector representing the local 'up' direction.
  , rotation :: Vector4    -- ^ Unit vector representing the local 'in' direction.
  , keys :: S.Set Key      -- ^ Currently pressed movement keys.
  , angle1 :: Float        -- ^ Sideways movement of the viewing cube.
  , angle2 :: Float        -- ^ Up movement of the viewing cube.
  , debug :: Bool          -- ^ Toggle for debug view.
  } deriving Show

-- | Calculates the unit vector corresponding to the local 'x' direction for a
-- given CameraState.
xAxis :: CameraState -> Vector4
xAxis cam = ternaryCross w y z
  where
    w = rotation cam
    y = orientation cam
    z = direction cam

initialState :: CameraState
initialState = Position
  { location = (0,0,0,-200)
  , direction = (0,0,0,1)
  , orientation = (0,0,1,0)
  , rotation = (1,0,0,0)
  , angle1 = degToRad 20
  , angle2 = degToRad 20
  , keys = S.empty
  , debug = False
  }

-----------------------------
-- Vectors
-----------------------------

type Vector3 = (Float,Float,Float)

-- | Multiply a 3-dimensional vector by a scalar.
mulSV3 :: Float -> Vector3 -> Vector3
mulSV3 a (x,y,z) = (a*x,a*y,a*z)

-- | Add two 3-dimensional vectors together.
addV3 :: Vector3 -> Vector3 -> Vector3
addV3 (x,y,z) (x',y',z') = (x+x',y+y',z+z')

-- | Negate a 3-dimensional vector.
negV3 :: Vector3 -> Vector3
negV3 = mulSV3 (-1)

-- | Subtract one 3-dimensional vector from another.
subV3 :: Vector3 -> Vector3 -> Vector3
subV3 v = addV3 v . negV3

-- | Get the components of a 3-dimensional vector as individual vectors.
componentsV3 :: Vector3 -> [Vector3]
componentsV3 (x,y,z) = [(x,0,0), (0,y,0), (0,0,z)]

-- | Find the scalar representing the point of intersection of another (3D)
-- vector with a plane in vector form.
-- Will fail if the line is parallel to the plane.
vplIntersectionV3 :: (Vector3,Vector3) -> (Vector3,Vector3) -> Vector3 -> Float
vplIntersectionV3 (v,vpos) (pl1, pl2) plpos = -solution
  where
    coefficients = [pl1,pl2,mulSV3 (-1) v]
    variables = plpos `subV3` vpos
    cols = map tuple3ToList coefficients ++ [tuple3ToList variables]
    augmat = fromLists $ transpose cols
    solution = either error (getElem 3 4) (rref augmat)

plane3ToVectors :: Plane -> (Vector3,Vector3)
plane3ToVectors plane =
  case plane of
    WX -> ((1,0,0),(0,1,0))
    WY -> ((1,0,0),(0,0,1))
    XY -> ((0,1,0),(0,0,1))
    _ -> error (shows plane " cannot be expressed in terms of Vector3")

type Vector4 = (Float,Float,Float,Float)

-- | Just a unit vector that points in the global up direction
upV4 :: Vector4
upV4 = (0,0,1,0)

-- | Likewise, just a unit vector pointing in the global 'in' direction
inV4 :: Vector4
inV4 = (1,0,0,0)

-- | Produce a 4-dimensional vector perpendicular to 3 other vectors.
ternaryCross :: Vector4 -> Vector4 -> Vector4 -> Vector4
ternaryCross v1 v2 v3 =
  (detnth 0, -(detnth 1), detnth 2, -(detnth 3))
  where
    concatv = concatMap tuple4ToList [v1,v2,v3]
    matrices = map (fromList 3 3) concatvFilts
    concatvFilts = [[x | (i,x) <- zip [0..] concatv, i `mod` 4 /= a]
                       | a <- [0..]]
    detnth n = detLaplace (matrices!!n)

-- | Find the projection of 4-dimensional vectors u on v.
projV4 :: Vector4 -> Vector4 -> Vector4
projV4 (0,0,0,0) _ = (0,0,0,0)
projV4 u v = ((u `dotV4` v) / magV4 u*magV4 u) `mulSV4` u

-- | Calculate the dot product of 4-dimensional vectors.
dotV4 :: Vector4 -> Vector4 -> Float
dotV4 (w,x,y,z) (w',x',y',z') = w*w'+x*x'+y*y'+z*z'

-- | Add two four-dimensional vectors together
addV4 :: Vector4 -> Vector4 -> Vector4
addV4 (w,x,y,z) (w',x',y',z') = (w+w',x+x',y+y',z+z')

-- | Subtract a 4D vector from another 4D vector
subV4 :: Vector4 -> Vector4 -> Vector4
subV4 v = addV4 v . negV4

-- | Magnitude of a 4-dimensional vector
magV4 :: Vector4 -> Float
magV4 (w,x,y,z) = sqrt (w*w + x*x + y*y + z*z)

-- | Multiply a 4-dimensional vector by a scalar.
mulSV4 :: Float -> Vector4 -> Vector4
mulSV4 a (w,x,y,z) = (a*w,a*x,a*y,a*z)

-- | Negate a 4-dimensional vector.
negV4 :: Vector4 -> Vector4
negV4 = mulSV4 (-1)

-- | Normalise a 4-dimensional vector, so it has a magnitude of 1.
normalizeV4 :: Vector4 -> Vector4
normalizeV4 v = mulSV4 (1 / magV4 v) v

distV4 :: Vector4 -> Vector4 -> Float
distV4 v1 v2 = magV4 (v2 `subV4` v1)

-- | W - in/out, 
-- X - right/left,
-- Y - up/down,
-- Z - forward/backward
data Plane = WX | WY | WZ | XY | XZ | YZ deriving Show

-- | Rotate a 4-dimensional vector by an angle in radians along the specified
-- plane, relative to CameraState.
rotateV4Relative :: CameraState -> Plane -> Float -> Vector4 -> Vector4
rotateV4Relative cam plane a = 
  listToTuple4 . toList 
  . multStd (vMatrix `multStd` planeRot plane a) 
  . relativeCoordinates vMatrix
  where
    vMatrix = camMatrix cam

-- | Takes a CameraState, converts the camera's position to a Matrix Float,
-- where each column corresponds to one of the unit vector's stored in
-- CameraState.
camMatrix :: CameraState -> Matrix Float
camMatrix cam = fromLists . transpose $ map tuple4ToList [localW, localX, localY, localZ]
  where
    (localW,localX,localY,localZ) =
      (rotation cam, xAxis cam, orientation cam, direction cam)
-- | Takes a matrix of linearly independent columns and a vector, expresses 
-- that vector as a linear combination of the matrix columns in the form of a 
-- column matrix.
relativeCoordinates :: Matrix Float -> Vector4 -> Matrix Float
relativeCoordinates m v = either error (submatrix 1 4 5 5) (rref m')
  where
    v' = fromLists . transpose $ [tuple4ToList v]
    m' = m <|> v'

-- | Takes a CameraState and a vector, returns the vector's relative position
-- to the camera, with respect to location, direction, orientation and rotation.
relativeV4 :: CameraState -> Vector4 -> Vector4
relativeV4 cam = listToTuple4 . toList . relativeCoordinates (camMatrix cam) . negV4 . subV4 (location cam)
    

-----------------------------
-- Rotation matrices
-----------------------------

planeRot :: Plane -> Float -> Matrix Float
planeRot plane = case plane of
  WX -> rotWX
  WY -> rotWY
  WZ -> rotWZ
  XY -> rotXY
  XZ -> rotXZ
  YZ -> rotYZ

rotWX, rotWY, rotWZ, rotXY, rotXZ, rotYZ :: Float -> Matrix Float
rotWX a = fromLists [[cos a, sin a, 0, 0],[-sin a, cos a,0,0],[0,0,1,0],[0,0,0,1]]
rotWY a = fromLists [[cos a,0,-sin a,0],[0,1,0,0],[sin a,0,cos a,0],[0,0,0,1]]
rotWZ a = fromLists [[cos a,0,0,sin a],[0,1,0,0],[0,0,1,0],[-sin a,0,0,cos a]]
rotXY a = fromLists [[1,0,0,0],[0,cos a, sin a, 0],[0,-sin a, cos a,0],[0,0,0,1]]
rotXZ a = fromLists [[1,0,0,0],[0,cos a,0,-sin a],[0,0,1,0],[0,sin a,0,cos a]]
rotYZ a = fromLists [[1,0,0,0],[0,1,0,0],[0,0,cos a, -sin a],[0,0,sin a, cos a]]


-----------------------------
-- Tuples
-----------------------------

tuple3ToList :: (a, a, a) -> [a]
tuple3ToList (a,b,c) = [a,b,c]

listToTuple3 :: [[c]] -> (c, c, c)
listToTuple3 [a:b:c:_] = (a,b,c)
listToTuple3 _ = error "List contains less than 3 elements, cannot convert to 3-tuple"

tuple4ToList :: (a, a, a, a) -> [a]
tuple4ToList (a,b,c,d) = [a,b,c,d]

listToTuple4 :: [d] -> (d, d, d, d)
listToTuple4 (a:b:c:d:_) = (a,b,c,d)
listToTuple4 _ = error "List contains less than 4 elements, cannot convert to 4-tuple"