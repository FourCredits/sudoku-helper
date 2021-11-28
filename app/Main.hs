module Main where

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import Rendering
import Solving

main :: IO ()
main = renderSVG "output.svg" (dims2D 400 400) (renderGrid testGrid2)
