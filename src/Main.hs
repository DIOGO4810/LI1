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
  (650, 0)

bgColor :: Color
bgColor = black

fr :: Int
fr = 60

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
  putStrLn ("velocidade: " ++ (show $ velocidade $ jogador $ jogo state))
  putStrLn ("emEscada: " ++ (show $ emEscada $ jogador $ jogo state))

  return (drawGame state)

loadImages :: State -> IO State
loadImages state = do
  mario <- loadBMP "assets/marioparado.bmp"
  plataforma <- loadBMP "assets/plataforma.bmp"
  alcapao <- loadBMP "assets/alcapao.bmp"
  escada <- loadBMP "assets/escada.bmp"
  martelo <- loadBMP "assets/martelo.bmp"
  moeda <- loadBMP "assets/moeda.bmp"
  fantasma <- loadBMP "assets/fantasma1.bmp"

  return state {
    images = [
      ("mario",mario),
      ("plataforma",plataforma),
      ("alcapao",alcapao),
      ("escada",escada),
      ("martelo",martelo),
      ("moeda",moeda),
      ("fantasma", fantasma)
      ]
    }

main :: IO ()
main = do
  initState <- loadImages initialState
  playIO window bgColor fr initState draw react time