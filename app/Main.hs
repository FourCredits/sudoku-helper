module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Sudoku Helper" (800, 800) (10, 10)

backgroundColor :: Color
backgroundColor = white

outsideBorder :: Picture
outsideBorder = pictures
  [ color black $ rectangleSolid size size
  , color white $ rectangleSolid (size - thickness) (size - thickness)]
  where
    size = 400
    -- should be even
    thickness = 6

caption :: Picture
caption = translate (-200) (-200) $ scale factor factor $ text "hello, world!"
  where
    factor = 0.3

picture :: Picture
picture = pictures [outsideBorder, caption]

main :: IO ()
main = display window backgroundColor picture
