module Main where

import LI12324
import Niveis
import Tarefa3
import Tarefa4
import Utilities
import DrawMap
import DrawMenu
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tarefa3 (movimenta)
import GHC.Float (double2Float, float2Double)
import System.Exit
import System.Random (randomRIO)
import Utilities (Theme(MarioCat))

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
react (EventKey (SpecialKey KeyEsc) Down _ _) jogo = exitSuccess
react (EventKey (Char 'p') Down _ _) state = return $ state {currentMenu = Pause}
react e state 
  | currentMenu state == InGame = return $ state {levelsList = updateLevel (levelsList state) (currentLevel state,jogoS)}
  | currentMenu state == Pause = return $ reactPause e state
  | currentMenu state == GameOver = return $ reactGameOver e state
  | currentMenu state == Options = return $ reactOptions e state
  | otherwise = return $ reactMenu e state
  where jogoS = reactInGame e (jogo)
        jogo = (levelsList state) !! currentLevel state


reactInGame :: Event -> Jogo -> Jogo
reactInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just AndarDireita) jogo
reactInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
reactInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Subir) jogo
reactInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Descer) jogo
reactInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza [Nothing, Nothing, Nothing] (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo =  atualiza [Nothing, Nothing, Nothing] (Just Saltar) jogo
reactInGame event jogo = jogo

timeInGame :: Float -> State -> IO State
timeInGame tempo state = do
  seedGenerator <- randomRIO (1,100 :: Int)
  if exitGame state 
    then exitSuccess 
  else if (vida $ jogador $ jogo) == 0
    then return state {currentMenu=GameOver}
  else if (posicao $ jogador $ jogo) == (posf)
    then return $ state {currentLevel = currentLevel state + 1}
  else if currentMenu state == InGame
    then 
    return $ state {
    levelsList = updateLevel (levelsList state) (currentLevel state,movimenta seedGenerator (float2Double tempo) (jogo)),
    time = time state + float2Double tempo
    }
  else return state
  where (Mapa (posi,diri) posf blocos) = mapa $ jogo
        jogo = (levelsList state) !! currentLevel state


draw :: State -> IO Picture
draw state = do

  putStrLn ("pontos: " ++ (show $ pontos $ jogador $ jogo))
  putStrLn ("vida: " ++ (show $ vida $ jogador $ jogo))
  putStrLn ("armado: " ++ (show $ aplicaDano $ jogador $ jogo))
  putStrLn ("velocidade: " ++ (show $ velocidade $ jogador $ jogo))
  putStrLn ("velocidadeInims: " ++ (show (map velocidade (inimigos $ jogo))))
  putStrLn ("emEscadaInims: " ++ (show (map emEscada (inimigos $ jogo))))
  putStrLn ("emEscada: " ++ (show $ emEscada $ jogador $ jogo))
  putStrLn ("posicao: " ++ (show $ posicao $ jogador $ jogo))
  putStrLn ("emEscadaInims: " ++ (show (map (podeDescer (mapa $ jogo)) (inimigos $ jogo))))
  putStrLn ("selected: " ++ (show (selectedButton state)))

  if currentMenu state == InGame
    then return (drawGame state)
  else return (drawMenu state)
  where jogo = (levelsList state) !! currentLevel state

  

loadImages :: State -> IO State
loadImages state = do
  -- ! Assets Gerais
  heart <- loadBMP "assets/heart.bmp"
  martelo <- loadBMP "assets/martelo.bmp"
  moeda <- loadBMP "assets/moeda.bmp"
  zero <- loadBMP "assets/0.bmp"
  um <- loadBMP "assets/1.bmp"
  dois <- loadBMP "assets/2.bmp"
  tres <- loadBMP "assets/3.bmp"
  quatro <- loadBMP "assets/4.bmp"
  cinco <- loadBMP "assets/5.bmp"
  seis <- loadBMP "assets/6.bmp"
  sete <- loadBMP "assets/7.bmp"
  oito <- loadBMP "assets/8.bmp"
  nove <- loadBMP "assets/9.bmp"
  botao1Home <- loadBMP "assets/menuplay.bmp"
  botao2Home <- loadBMP "assets/menuoption.bmp"
  botao3Home <- loadBMP "assets/menuexit.bmp"
  botao1Options <- loadBMP "assets/botao1Options.bmp"
  botao2Options <- loadBMP "assets/botao2Options.bmp"
  botao3Options <- loadBMP "assets/botao3Options.bmp"
  botao4Options <- loadBMP "assets/botao4Options.bmp"
  botao5Options <- loadBMP "assets/botao5Options.bmp"
  botao1Pause <- loadBMP "assets/pausedresume.bmp"
  botao2Pause <- loadBMP "assets/pausedrestart.bmp"
  botao3Pause <- loadBMP "assets/pausedmenu.bmp"
  botao1GameOver <- loadBMP "assets/gameoverplayagain.bmp"
  botao2GameOver <- loadBMP "assets/gameovermenu.bmp"
  -- ! Mario
  marioparado <- loadBMP "assets/marioparado.bmp"
  marioandar1 <- loadBMP "assets/marioandar1.bmp"
  marioandar2 <- loadBMP "assets/marioandar2.bmp"
  marioescada <- loadBMP "assets/marioescada.bmp"
  mariosaltar <- loadBMP "assets/mariosaltar.bmp"
  plataforma <- loadBMP "assets/plataforma.bmp"
  alcapao <- loadBMP "assets/alcapao.bmp"
  escada <- loadBMP "assets/escada.bmp"
  estrela <- loadBMP "assets/estrela.bmp"
  fantasma1 <- loadBMP "assets/fantasma1.bmp"
  fantasma2 <- loadBMP "assets/fantasma2.bmp"
  -- ! MarioCat
  marioparadoCat <- loadBMP "assets/marioparadoCat.bmp"
  marioandar1Cat <- loadBMP "assets/marioandar1Cat.bmp"
  marioandar2Cat <- loadBMP "assets/marioandar2Cat.bmp"
  marioescadaCat <- loadBMP "assets/marioescadaCat.bmp"
  mariosaltarCat <- loadBMP "assets/mariosaltarCat.bmp"
  plataformaCat <- loadBMP "assets/plataformaCat.bmp"
  alcapaoCat <- loadBMP "assets/alcapaoCat.bmp"
  escadaCat <- loadBMP "assets/escadaCat.bmp"
  estrelaCat <- loadBMP "assets/estrelaCat.bmp"
  fantasma1Cat <- loadBMP "assets/fantasma1Cat.bmp"
  fantasma2Cat <- loadBMP "assets/fantasma2Cat.bmp"
  -- ! MarioBear
  marioparadoBear <- loadBMP "assets/marioparadoBear.bmp"
  marioandar1Bear <- loadBMP "assets/marioandar1Bear.bmp"
  marioandar2Bear <- loadBMP "assets/marioandar2Bear.bmp"
  marioescadaBear <- loadBMP "assets/marioescadaBear.bmp"
  mariosaltarBear <- loadBMP "assets/mariosaltarBear.bmp"
  plataformaBear <- loadBMP "assets/plataformaBear.bmp"
  alcapaoBear <- loadBMP "assets/alcapaoBear.bmp"
  escadaBear <- loadBMP "assets/escadaBear.bmp"
  estrelaBear <- loadBMP "assets/estrelaBear.bmp"
  fantasma1Bear <- loadBMP "assets/fantasma1Bear.bmp"
  fantasma2Bear <- loadBMP "assets/fantasma2Bear.bmp"
  -- ! MarioFrog
  marioparadoFrog <- loadBMP "assets/marioparadoFrog.bmp"
  marioandar1Frog <- loadBMP "assets/marioandar1Frog.bmp"
  marioandar2Frog <- loadBMP "assets/marioandar2Frog.bmp"
  marioescadaFrog <- loadBMP "assets/marioescadaFrog.bmp"
  mariosaltarFrog <- loadBMP "assets/mariosaltarFrog.bmp"
  plataformaFrog <- loadBMP "assets/plataformaFrog.bmp"
  alcapaoFrog <- loadBMP "assets/alcapaoFrog.bmp"
  escadaFrog <- loadBMP "assets/escadaFrog.bmp"
  estrelaFrog <- loadBMP "assets/estrelaFrog.bmp"
  fantasma1Frog <- loadBMP "assets/fantasma1Frog.bmp"
  fantasma2Frog <- loadBMP "assets/fantasma2Frog.bmp"
  -- ! MarioAstronaut
  marioparadoAstronaut <- loadBMP "assets/marioparadoAstronaut.bmp"
  marioandar1Astronaut <- loadBMP "assets/marioandar1Astronaut.bmp"
  marioandar2Astronaut <- loadBMP "assets/marioandar2Astronaut.bmp"
  marioescadaAstronaut <- loadBMP "assets/marioescadaAstronaut.bmp"
  mariosaltarAstronaut <- loadBMP "assets/mariosaltarAstronaut.bmp"
  plataformaAstronaut <- loadBMP "assets/plataformaAstronaut.bmp"
  alcapaoAstronaut <- loadBMP "assets/alcapaoAstronaut.bmp"
  escadaAstronaut <- loadBMP "assets/escadaAstronaut.bmp"
  estrelaAstronaut <- loadBMP "assets/estrelaAstronaut.bmp"
  fantasma1Astronaut <- loadBMP "assets/fantasma1Astronaut.bmp"
  fantasma2Astronaut <- loadBMP "assets/fantasma2Astronaut.bmp"


  return state {
    images = [
      (Mario,[
        -- ! Geral
        ("heart",heart),
        ("0",zero),
        ("1",um),
        ("2",dois),
        ("3",tres),
        ("4",quatro),
        ("5",cinco),
        ("6",seis),
        ("7",sete),
        ("8",oito),
        ("9",nove),
        ("martelo",martelo),
        ("moeda",moeda),
        ("botao1Home", botao1Home),
        ("botao2Home", botao2Home),
        ("botao3Home", botao3Home),
        ("botao1Options", botao1Options),
        ("botao2Options", botao2Options),
        ("botao3Options", botao3Options),
        ("botao4Options", botao4Options),
        ("botao5Options", botao5Options),
        ("botao1Pause", botao1Pause),
        ("botao2Pause", botao2Pause),
        ("botao3Pause", botao3Pause),
        ("botao1GameOver", botao1GameOver),
        ("botao2GameOver", botao2GameOver),
        -- ! Mario
        ("marioparado",marioparado),
        ("marioandar1",marioandar1),
        ("marioandar2",marioandar2),
        ("marioescada",marioescada),
        ("mariosaltar",mariosaltar),
        ("plataforma",plataforma),
        ("alcapao",alcapao), 
        ("escada",escada),
        ("estrela",estrela),
        ("fantasma1", fantasma1),
        ("fantasma2", fantasma2)
      ]),
      (MarioCat,[
        ("marioparado",marioparadoCat),
        ("marioandar1",marioandar1Cat),
        ("marioandar2",marioandar2Cat),
        ("marioescada",marioescadaCat),
        ("mariosaltar",mariosaltarCat),
        ("plataforma",plataformaCat),
        ("alcapao",alcapaoCat), 
        ("escada",escadaCat),
        ("estrela",estrelaCat),
        ("fantasma1", fantasma1Cat),
        ("fantasma2", fantasma2Cat)
      ]),
      (MarioBear,[
        ("marioparado",marioparadoBear),
        ("marioandar1",marioandar1Bear),
        ("marioandar2",marioandar2Bear),
        ("marioescada",marioescadaBear),
        ("mariosaltar",mariosaltarBear),
        ("plataforma",plataformaBear),
        ("alcapao",alcapaoBear), 
        ("escada",escadaBear),
        ("estrela",estrelaBear),
        ("fantasma1", fantasma1Bear),
        ("fantasma2", fantasma2Bear)
      ]),
      (MarioFrog,[
        ("marioparado",marioparadoFrog),
        ("marioandar1",marioandar1Frog),
        ("marioandar2",marioandar2Frog),
        ("marioescada",marioescadaFrog),
        ("mariosaltar",mariosaltarFrog),
        ("plataforma",plataformaFrog),
        ("alcapao",alcapaoFrog), 
        ("escada",escadaFrog),
        ("estrela",estrelaFrog),
        ("fantasma1", fantasma1Frog),
        ("fantasma2", fantasma2Frog)
      ]),
      (MarioAstronaut,[
        ("marioparado",marioparadoAstronaut),
        ("marioandar1",marioandar1Astronaut),
        ("marioandar2",marioandar2Astronaut),
        ("marioescada",marioescadaAstronaut),
        ("mariosaltar",mariosaltarAstronaut),
        ("plataforma",plataformaAstronaut),
        ("alcapao",alcapaoAstronaut), 
        ("escada",escadaAstronaut),
        ("estrela",estrelaAstronaut),
        ("fantasma1", fantasma1Astronaut),
        ("fantasma2", fantasma2Astronaut)
      ])
      ]
    }

main :: IO ()
main = do
    initState <- loadImages initialState
    playIO window bgColor fr initState draw react timeInGame