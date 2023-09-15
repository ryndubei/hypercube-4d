module Main (main) where

import Graphics.Gloss

import Lib
import Input
import Math

windowWidth, windowHeight :: Int
windowWidth = 400
windowHeight = 400

background :: Color
background = black

window :: Display
window = InWindow "hypercube-4d" (windowWidth, windowHeight) (0,0)

main :: IO ()
main = play window background 60 initialState render handleKeys update
