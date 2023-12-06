module Main where

import LI12324
import Mapas
import DrawMap
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow
  "Primate Kong"
  windowSize
  (700, 150)

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

react :: Event -> Jogo -> IO Jogo
react event jogo = return jogo

time :: Float -> Jogo -> IO Jogo
time tempo jogo = return jogo

draw :: Jogo -> IO Picture
draw jogo = do
  return $ drawGame jogo

main :: IO ()
main = playIO window bgColor fr jogoSamp draw react time 
