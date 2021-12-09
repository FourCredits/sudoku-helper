module Main where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import Rendering
import TestGrids

main :: IO ()
main = renderSVG "output.svg" (dims2D 800 400) (renderSolution testGrid4)
