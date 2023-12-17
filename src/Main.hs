module Main where

import LI12324
import Mapas
import Tarefa4

import DrawMap
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tarefa3 (movimenta)
import GHC.Float (double2Float, float2Double)

window :: Display
window = InWindow
  "Primate Kong"
  windowSize
  (700, 150)

bgColor :: Color
bgColor = black

fr :: Int
fr = 120

react :: Event -> Jogo -> IO Jogo
react (EventKey (SpecialKey KeyRight) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
react (EventKey (SpecialKey KeyRight) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
react (EventKey (SpecialKey KeyLeft) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
react (EventKey (SpecialKey KeyLeft) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
react (EventKey (SpecialKey KeyUp) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
react (EventKey (SpecialKey KeyUp) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
react (EventKey (SpecialKey KeyDown) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
react (EventKey (SpecialKey KeyDown) Up _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
react (EventKey (SpecialKey KeySpace) Down _ _) jogo = return $ atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
react event jogo = return jogo

time :: Float -> Jogo -> IO Jogo
time tempo jogo = return $ movimenta 1 (float2Double tempo) jogo

draw :: [(String,Picture)] -> Jogo -> IO Picture
draw images jogo = do
  return $ drawGame jogo images

loadImages :: IO [(String,Picture)]
loadImages = do
  mario <- loadBMP "assets/Marioandar.bmp"
  plataforma <- loadBMP "assets/Plataforma.bmp"
  alcapao <- loadBMP "assets/Alcapao.bmp"
  escada <- loadBMP "assets/ladder.bmp"
  martelo <- loadBMP "assets/Martelo.bmp"
  moeda <- loadBMP "assets/Moeda.bmp"

  return [("mario",mario),("plataforma",plataforma),("alcapao",alcapao),("escada",escada),("martelo",martelo),("moeda",moeda)]

main :: IO ()
main = do
  images <- loadImages
  playIO window bgColor fr jogoSamp (draw images) react time