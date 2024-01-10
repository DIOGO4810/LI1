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
react (EventKey (Char 't') Down _ _) state 
  | currentTheme state == Mario = return $ state {currentTheme=MarioCat}
  | currentTheme state == MarioCat = return $ state {currentTheme=MarioBear}
  | currentTheme state == MarioBear = return $ state {currentTheme=MarioFrog}
  | currentTheme state == MarioFrog = return $ state {currentTheme=MarioAstronaut}
  | currentTheme state == MarioAstronaut = return $ state {currentTheme=Mario}
react (EventKey (Char 'm') Down _ _) state 
  | currentMode state == Easy = return $ state {currentMode=Medium}
  | currentMode state == Medium = return $ state {currentMode=Hard}
  | currentMode state == Hard = return $ state {currentMode=Easy}
react (EventKey (Char 'p') Down _ _) state = if (currentMenu state)== InGame then return $ state {currentMenu = Pause} else return state
react e state 
  | currentMenu state == InGame = return $ state {levelsList = updateLevel (levelsList state) (currentLevel state,jogoS)}
  | currentMenu state == Pause = return $ reactPause e state
  | currentMenu state == Options = return $ reactOptions e state
  | currentMenu state == Mode = return $ reactMode e state
  | currentMenu state == Themes = return $ reactThemes e state
  | currentMenu state == GameOver = return $ reactGameOver e state
  | otherwise = return $ reactMenu e state
  where jogoS = reactInGame e (jogo)
        jogo = (levelsList state) !! currentLevel state


reactInGame :: Event -> Jogo -> Jogo
reactInGame (EventKey (Char 'd') Down _ _) jogo =  atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarDireita) jogo
reactInGame (EventKey (Char 'd') Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (Char 'a') Down _ _) jogo =  atualiza (replicate (length (inimigos jogo)) Nothing) (Just AndarEsquerda) jogo
reactInGame (EventKey (Char 'a') Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (Char 'w') Down _ _) jogo =  atualiza (replicate (length (inimigos jogo)) Nothing) (Just Subir) jogo
reactInGame (EventKey (Char 'w') Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (Char 's') Down _ _) jogo =  atualiza (replicate (length (inimigos jogo)) Nothing) (Just Descer) jogo
reactInGame (EventKey (Char 's') Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza (replicate (length (inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeySpace) Down _ _) jogo =  atualiza (replicate (length (inimigos jogo)) Nothing) (Just Saltar) jogo
reactInGame (EventKey (SpecialKey KeyDelete) Down _ _) jogo =  Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = listaColecionaveis, inimigos = listaInimigos}
  where           
                  (vx,vy) = velocidade jogador1
                  (px,py) = posicao jogador1
                  inEscada = emEscada jogador1
                  pontosjogador = pontos jogador1
                  (tamanhoX,tamanhoY) = tamanho jogador1
                  ressaltando = ressalta jogador1
                  direcaojogador = direcao jogador1
                  listaInimigos = inimigos jogo
                  listaColecionaveis = colecionaveis jogo
                  vidajogador = vida jogador1
                  impulsaojogador = impulsao jogador1
                  (maybeEscudo,tempoescudo) = escudo jogador1
                  jogador1 = jogador jogo
                  mapa1 = mapa jogo
                  personagem1 = Personagem {direcao = direcaojogador , emEscada = inEscada, velocidade = (vx,vy),posicao = (px,py),tamanho =(tamanhoX,tamanhoY) , ressalta = ressaltando, tipo = Jogador, vida = vidajogador, aplicaDano = (True, 100), pontos = pontosjogador,escudo = (maybeEscudo,tempoescudo), impulsao = impulsaojogador}
                  jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = listaColecionaveis, inimigos = listaInimigos}
reactInGame (EventKey (SpecialKey KeyInsert) Down _ _) jogo =  Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = listaColecionaveis, inimigos = listaInimigos}
  where           
                  (vx,vy) = velocidade jogador1
                  (px,py) = posicao jogador1
                  pontosjogador = pontos jogador1
                  inEscada = emEscada jogador1
                  impulsaojogador = impulsao jogador1
                  (maybeEscudo,tempoescudo) = escudo jogador1
                  (tamanhoX,tamanhoY) = tamanho jogador1
                  ressaltando = ressalta jogador1
                  direcaojogador = direcao jogador1
                  listaInimigos = inimigos jogo
                  listaColecionaveis = colecionaveis jogo
                  vidajogador = vida jogador1
                  jogador1 = jogador jogo
                  mapa1 = mapa jogo
                  personagem1 = Personagem {direcao = direcaojogador , emEscada = inEscada, velocidade = (vx,vy),posicao = (px,py),tamanho =(tamanhoX,tamanhoY) , ressalta = ressaltando, tipo = Jogador, vida = vidajogador, aplicaDano = (True, 100), pontos = pontosjogador,escudo = (maybeEscudo,tempoescudo), impulsao = impulsaojogador}
                  jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = listaColecionaveis, inimigos = listaInimigos}

reactInGame (EventKey (SpecialKey KeyRight) Down _ _) jogo =  atualiza (replicate (length(inimigos jogo)) Nothing) (Just AndarDireita) jogo
reactInGame (EventKey (SpecialKey KeyRight) Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza (replicate (length(inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyLeft) Down _ _) jogo =  atualiza (replicate (length(inimigos jogo)) Nothing) (Just AndarEsquerda) jogo
reactInGame (EventKey (SpecialKey KeyLeft) Up _ _) jogo =  if ((fst $ velocidade $ jogador jogo) /= -4.5 && (fst $ velocidade $ jogador jogo) /= 4.5) then atualiza (replicate (length(inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyUp) Down _ _) jogo =  atualiza (replicate (length(inimigos jogo)) Nothing) (Just Subir) jogo
reactInGame (EventKey (SpecialKey KeyUp) Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza (replicate (length(inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame (EventKey (SpecialKey KeyDown) Down _ _) jogo =  atualiza (replicate (length(inimigos jogo)) Nothing) (Just Descer) jogo
reactInGame (EventKey (SpecialKey KeyDown) Up _ _) jogo =  if (emEscada $ jogador jogo) then atualiza (replicate (length(inimigos jogo)) Nothing) (Just Parar) jogo else jogo
reactInGame event jogo = jogo

              


timeInGame :: Float -> State -> IO State
timeInGame tempo state = do
  seedGenerator <- randomRIO (1,100 :: Int)
  if exitGame state 
    then do
      writeFile "highscore.txt" (show (highScore state))
      exitSuccess 
  else if (vida $ jogador jogo) == 0
    then return state {currentMenu=GameOver}
  else if ( fromIntegral $ floor px,fromIntegral $ floor py)  == (fromIntegral $ floor xf,fromIntegral $ floor yf)
    then return $ state {currentLevel = currentLevel state + 1}
  else if currentMenu state == InGame
    then 
    return $ state {
    levelsList = updateLevel (levelsList state) (currentLevel state, (movimenta seedGenerator (float2Double tempo) jogo)),
    time = time state + float2Double tempo,
    currentPoints = sum (map (\jogo -> pontos $ jogador jogo) (levelsList state)),
    highScore = max (currentPoints state) (highScore state)
    }
  else return state
  where (Mapa (posi,diri) (xf,yf) blocos) = mapa $ jogo
        jogo = (levelsList state) !! currentLevel state
        (px,py) = (posicao $ jogador jogo)

    


draw :: State -> IO Picture
draw state = do

  putStrLn ("pontos: " ++ (show $ pontos $ jogador $ jogo))
  putStrLn ("vida: " ++ (show $ vida $ jogador $ jogo))
  putStrLn ("armado: " ++ (show $ aplicaDano $ jogador $ jogo))
  putStrLn ("velocidade: " ++ (show $ velocidade $ jogador $ jogo))
  putStrLn ("emEscada: " ++ (show $ emEscada $ jogador $ jogo))
  putStrLn ("impulsao: " ++ (show $ impulsao $ jogador $ jogo))
  putStrLn ("posicao: " ++ (show $ posicao $ jogador $ jogo))
  putStrLn ("escudo: " ++ (show $ escudo $ jogador $ jogo))


  if currentMenu state == InGame
    then return (drawGame state)
  else return (drawMenu state)
  where jogo = (levelsList state) !! currentLevel state

  

loadImages :: State -> IO State
loadImages state = do
  -- ! Assets Gerais
  heart <- loadBMP "assets/heart.bmp"
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
  escuro <- loadBMP "assets/escuro.bmp"
  botao1Home <- loadBMP "assets/menuplay.bmp"
  botao2Home <- loadBMP "assets/menuoptions.bmp"
  botao3Home <- loadBMP "assets/menuexit.bmp"
  botao1Options <- loadBMP "assets/botao1Options.bmp"
  botao2Options <- loadBMP "assets/botao2Options.bmp"
  botao3Options <- loadBMP "assets/botao3Options.bmp"
  botao1Mode <- loadBMP "assets/botao1Mode.bmp"
  botao2Mode <- loadBMP "assets/botao2Mode.bmp"
  botao3Mode <- loadBMP "assets/botao3Mode.bmp"
  botao1Themes <- loadBMP "assets/botao1Themes.bmp"
  botao2Themes <- loadBMP "assets/botao2Themes.bmp"
  botao3Themes <- loadBMP "assets/botao3Themes.bmp"
  botao4Themes <- loadBMP "assets/botao4Themes.bmp"
  botao5Themes <- loadBMP "assets/botao5Themes.bmp"
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
  trampolim <- loadBMP "assets/trampolim.bmp"
  alcapao <- loadBMP "assets/alcapao.bmp"
  escada <- loadBMP "assets/escada.bmp"
  estrela <- loadBMP "assets/estrela.bmp"
  martelo <- loadBMP "assets/martelo.bmp"
  shield <- loadBMP "assets/escudo.bmp"
  fantasma1 <- loadBMP "assets/fantasma1.bmp"
  fantasma2 <- loadBMP "assets/fantasma2.bmp"
  donkeykong1 <- loadBMP "assets/donkeykong1.bmp"
  donkeykong2 <- loadBMP "assets/donkeykong2.bmp"
  barril1 <- loadBMP "assets/barril1.bmp"
  barril2 <- loadBMP "assets/barril2.bmp"
  -- ! MarioCat
  marioparadoCat <- loadBMP "assets/marioparadoCat.bmp"
  marioandar1Cat <- loadBMP "assets/marioandar1Cat.bmp"
  marioandar2Cat <- loadBMP "assets/marioandar2Cat.bmp"
  marioescadaCat <- loadBMP "assets/marioescadaCat.bmp"
  mariosaltarCat <- loadBMP "assets/mariosaltarCat.bmp"
  plataformaCat <- loadBMP "assets/plataformaCat.bmp"
  trampolimCat <- loadBMP "assets/trampolimCat.bmp"
  alcapaoCat <- loadBMP "assets/alcapaoCat.bmp"
  escadaCat <- loadBMP "assets/escadaCat.bmp"
  estrelaCat <- loadBMP "assets/estrelaCat.bmp"
  marteloCat <- loadBMP "assets/marteloCat.bmp"
  shieldCat <- loadBMP "assets/escudoCat.bmp"
  fantasma1Cat <- loadBMP "assets/fantasma1Cat.bmp"
  fantasma2Cat <- loadBMP "assets/fantasma2Cat.bmp"
  donkeykong1Cat <- loadBMP "assets/donkeykong1Cat.bmp"
  donkeykong2Cat <- loadBMP "assets/donkeykong2Cat.bmp"
  barril1Cat <- loadBMP "assets/barril1Cat.bmp"
  barril2Cat <- loadBMP "assets/barril2Cat.bmp"
  -- ! MarioBear
  marioparadoBear <- loadBMP "assets/marioparadoBear.bmp"
  marioandar1Bear <- loadBMP "assets/marioandar1Bear.bmp"
  marioandar2Bear <- loadBMP "assets/marioandar2Bear.bmp"
  marioescadaBear <- loadBMP "assets/marioescadaBear.bmp"
  mariosaltarBear <- loadBMP "assets/mariosaltarBear.bmp"
  plataformaBear <- loadBMP "assets/plataformaBear.bmp"
  trampolimBear <- loadBMP "assets/trampolimBear.bmp"
  alcapaoBear <- loadBMP "assets/alcapaoBear.bmp"
  escadaBear <- loadBMP "assets/escadaBear.bmp"
  estrelaBear <- loadBMP "assets/estrelaBear.bmp"
  marteloBear <- loadBMP "assets/marteloBear.bmp"
  shieldBear <- loadBMP "assets/escudoBear.bmp"
  fantasma1Bear <- loadBMP "assets/fantasma1Bear.bmp"
  fantasma2Bear <- loadBMP "assets/fantasma2Bear.bmp"
  donkeykong1Bear <- loadBMP "assets/donkeykong1Bear.bmp"
  donkeykong2Bear <- loadBMP "assets/donkeykong2Bear.bmp"
  barril1Bear <- loadBMP "assets/barril1Bear.bmp"
  barril2Bear <- loadBMP "assets/barril2Bear.bmp"
  -- ! MarioFrog
  marioparadoFrog <- loadBMP "assets/marioparadoFrog.bmp"
  marioandar1Frog <- loadBMP "assets/marioandar1Frog.bmp"
  marioandar2Frog <- loadBMP "assets/marioandar2Frog.bmp"
  marioescadaFrog <- loadBMP "assets/marioescadaFrog.bmp"
  mariosaltarFrog <- loadBMP "assets/mariosaltarFrog.bmp"
  plataformaFrog <- loadBMP "assets/plataformaFrog.bmp"
  trampolimFrog <- loadBMP "assets/trampolimFrog.bmp"
  alcapaoFrog <- loadBMP "assets/alcapaoFrog.bmp"
  escadaFrog <- loadBMP "assets/escadaFrog.bmp"
  estrelaFrog <- loadBMP "assets/estrelaFrog.bmp"
  marteloFrog <- loadBMP "assets/marteloFrog.bmp"
  shieldFrog <- loadBMP "assets/escudoFrog.bmp"
  fantasma1Frog <- loadBMP "assets/fantasma1Frog.bmp"
  fantasma2Frog <- loadBMP "assets/fantasma2Frog.bmp"
  donkeykong1Frog <- loadBMP "assets/donkeykong1Frog.bmp"
  donkeykong2Frog <- loadBMP "assets/donkeykong2Frog.bmp"
  barril1Frog <- loadBMP "assets/barril1Frog.bmp"
  barril2Frog <- loadBMP "assets/barril2Frog.bmp"
  -- ! MarioAstronaut
  marioparadoAstronaut <- loadBMP "assets/marioparadoAstronaut.bmp"
  marioandar1Astronaut <- loadBMP "assets/marioandar1Astronaut.bmp"
  marioandar2Astronaut <- loadBMP "assets/marioandar2Astronaut.bmp"
  marioescadaAstronaut <- loadBMP "assets/marioescadaAstronaut.bmp"
  mariosaltarAstronaut <- loadBMP "assets/mariosaltarAstronaut.bmp"
  plataformaAstronaut <- loadBMP "assets/plataformaAstronaut.bmp"
  trampolimAstronaut <- loadBMP "assets/trampolimAstronaut.bmp"
  alcapaoAstronaut <- loadBMP "assets/alcapaoAstronaut.bmp"
  escadaAstronaut <- loadBMP "assets/escadaAstronaut.bmp"
  estrelaAstronaut <- loadBMP "assets/estrelaAstronaut.bmp"
  marteloAstronaut <- loadBMP "assets/marteloAstronaut.bmp"
  shieldAstronaut <- loadBMP "assets/escudoAstronaut.bmp"
  fantasma1Astronaut <- loadBMP "assets/fantasma1Astronaut.bmp"
  fantasma2Astronaut <- loadBMP "assets/fantasma2Astronaut.bmp"
  donkeykong1Astronaut <- loadBMP "assets/donkeykong1Astronaut.bmp"
  donkeykong2Astronaut <- loadBMP "assets/donkeykong2Astronaut.bmp"
  barril1Astronaut <- loadBMP "assets/barril1Astronaut.bmp"
  barril2Astronaut <- loadBMP "assets/barril2Astronaut.bmp"


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
        ("moeda",moeda),
        ("escuro",escuro),
        ("botao1Home", botao1Home),
        ("botao2Home", botao2Home),
        ("botao3Home", botao3Home),
        ("botao1Options", botao1Options),
        ("botao2Options", botao2Options),
        ("botao3Options", botao3Options),
        ("botao1Mode", botao1Mode),
        ("botao2Mode", botao2Mode),
        ("botao3Mode", botao3Mode),
        ("botao1Themes", botao1Themes),
        ("botao2Themes", botao2Themes),
        ("botao3Themes", botao3Themes),
        ("botao4Themes", botao4Themes),
        ("botao5Themes", botao5Themes),
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
        ("trampolim",trampolim),
        ("alcapao",alcapao), 
        ("escada",escada),
        ("estrela",estrela),
        ("martelo",martelo),
        ("shield",shield),
        ("fantasma1", fantasma1),
        ("fantasma2", fantasma2),
        ("donkeykong1", donkeykong1),
        ("donkeykong2", donkeykong2),
        ("barril1", barril1),
        ("barril2", barril2)
      ]),
      (MarioCat,[
        ("marioparado",marioparadoCat),
        ("marioandar1",marioandar1Cat),
        ("marioandar2",marioandar2Cat),
        ("marioescada",marioescadaCat),
        ("mariosaltar",mariosaltarCat),
        ("plataforma",plataformaCat),
        ("trampolim",trampolimCat),
        ("alcapao",alcapaoCat), 
        ("escada",escadaCat),
        ("estrela",estrelaCat),
        ("martelo",marteloCat),
        ("shield",shieldCat),
        ("fantasma1", fantasma1Cat),
        ("fantasma2", fantasma2Cat),
        ("donkeykong1", donkeykong1Cat),
        ("donkeykong2", donkeykong2Cat),
        ("barril1", barril1Cat),
        ("barril2", barril2Cat)
      ]),
      (MarioBear,[
        ("marioparado",marioparadoBear),
        ("marioandar1",marioandar1Bear),
        ("marioandar2",marioandar2Bear),
        ("marioescada",marioescadaBear),
        ("mariosaltar",mariosaltarBear),
        ("plataforma",plataformaBear),
        ("trampolim",trampolimBear),
        ("alcapao",alcapaoBear), 
        ("escada",escadaBear),
        ("estrela",estrelaBear),
        ("martelo",marteloBear),
        ("shield",shieldBear),
        ("fantasma1", fantasma1Bear),
        ("fantasma2", fantasma2Bear),
        ("donkeykong1", donkeykong1Bear),
        ("donkeykong2", donkeykong2Bear),
        ("barril1", barril1Bear),
        ("barril2", barril2Bear)
      ]),
      (MarioFrog,[
        ("marioparado",marioparadoFrog),
        ("marioandar1",marioandar1Frog),
        ("marioandar2",marioandar2Frog),
        ("marioescada",marioescadaFrog),
        ("mariosaltar",mariosaltarFrog),
        ("plataforma",plataformaFrog),
        ("trampolim",trampolimFrog),
        ("alcapao",alcapaoFrog), 
        ("escada",escadaFrog),
        ("estrela",estrelaFrog),
        ("martelo",marteloFrog),
        ("shield",shieldFrog),
        ("fantasma1", fantasma1Frog),
        ("fantasma2", fantasma2Frog),
        ("donkeykong1", donkeykong1Frog),
        ("donkeykong2", donkeykong2Frog),
        ("barril1", barril1Frog),
        ("barril2", barril2Frog)
      ]),
      (MarioAstronaut,[
        ("marioparado",marioparadoAstronaut),
        ("marioandar1",marioandar1Astronaut),
        ("marioandar2",marioandar2Astronaut),
        ("marioescada",marioescadaAstronaut),
        ("mariosaltar",mariosaltarAstronaut),
        ("plataforma",plataformaAstronaut),
        ("trampolim",trampolimAstronaut),
        ("alcapao",alcapaoAstronaut), 
        ("escada",escadaAstronaut),
        ("estrela",estrelaAstronaut),
        ("martelo",marteloAstronaut),
        ("shield",shieldAstronaut),
        ("fantasma1", fantasma1Astronaut),
        ("fantasma2", fantasma2Astronaut),
        ("donkeykong1", donkeykong1Astronaut),
        ("donkeykong2", donkeykong2Astronaut),
        ("barril1", barril1Astronaut),
        ("barril2", barril2Astronaut)
      ])
      ]
    }

main :: IO ()
main = do
    hs <- readFile "highscore.txt"
    initState <- loadImages initialState
    playIO window bgColor fr initState{highScore=read hs} draw react timeInGame