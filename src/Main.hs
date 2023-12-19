module Main where

import LI12324
import Mapas
import Tarefa4
import Utilities
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

react :: Event -> State -> IO State
react e state = return $ state {
  jogo = reactInGame e (jogo state)
}

reactInGame :: Event -> Jogo -> Jogo
reactInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
reactInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
reactInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
reactInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
reactInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
reactInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
reactInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
reactInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo
reactInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
reactInGame event jogo = jogo

time :: Float -> State -> IO State
time tempo state = return $ state {
  jogo = movimenta 1 (float2Double tempo) (jogo state)
  }

draw :: State -> IO Picture
draw state = do

  putStrLn ("pontos: " ++ (show $ pontos $ jogador $ jogo state))
  putStrLn ("vida: " ++ (show $ vida $ jogador $ jogo state))
  putStrLn ("armado: " ++ (show $ aplicaDano $ jogador $ jogo state))

  return (drawGame state)

loadImages :: State -> IO State
loadImages state = do
  mario <- loadBMP "assets/Marioandar.bmp"
  plataforma <- loadBMP "assets/Plataforma.bmp"
  alcapao <- loadBMP "assets/Alcapao.bmp"
  escada <- loadBMP "assets/ladder.bmp"
  martelo <- loadBMP "assets/Martelo.bmp"
  moeda <- loadBMP "assets/Moeda.bmp"

  return state {
    images = [
      ("mario",mario),
      ("plataforma",plataforma),
      ("alcapao",alcapao),
      ("escada",escada),
      ("martelo",martelo),
      ("moeda",moeda)
      ]
    }

main :: IO ()
main = do
  initState <- loadImages initialState
  playIO window bgColor fr initState draw react time