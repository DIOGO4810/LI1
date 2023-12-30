module DrawMap where

import LI12324
import Utilities
import Graphics.Gloss
import GHC.Float (double2Float)
import Data.Maybe
import Mapas
import Data.Fixed


scaleGame :: Int
scaleGame = 50

windowSize :: (Int,Int)
windowSize = (((length $ head blocos) * scaleGame),((length blocos) * scaleGame))
  where (Mapa _ _ blocos) = mapa1

posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (((double2Float x)*fromIntegral scaleGame)-(fromIntegral $ (fst windowSize))/2, ((fromIntegral $ (snd windowSize))/2 - (double2Float y) * fromIntegral scaleGame))

drawBlocks :: Mapa -> Images -> Picture
drawBlocks mapa images = Pictures [Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ plataforma) (mapaBlocos mapa Plataforma),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) alcapao) (mapaBlocos mapa Alcapao),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) escada) (mapaBlocos mapa Escada)]
  where plataforma = fromJust(lookup ("plataforma") images)
        alcapao = fromJust(lookup ("alcapao") images)
        escada = fromJust(lookup ("escada") images)

drawPlayer :: State -> Images -> Picture
drawPlayer state images = 
  if ((direcao $ jogador jogoD) == Norte  || (direcao $ jogador jogoD) == Sul) && (emEscada $ jogador jogoD)
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioescada
  else if (direcao $ jogador jogoD) == Oeste && (fst $ velocidade $ jogador jogoD) == 0 && (snd $ velocidade $ jogador jogoD) == 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioparado, martelo]
  else if ((direcao $ jogador jogoD) == Este || (direcao $ jogador jogoD) == Norte) && (fst $ velocidade $ jogador jogoD) == 0 && (snd $ velocidade $ jogador jogoD) == 0 
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado, martelo]
  else if (direcao $ jogador jogoD) == Oeste && (fst $ velocidade $ jogador jogoD) /= 0 && (snd $ velocidade $ jogador jogoD) == 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioandar,  martelo]
  else if (direcao $ jogador jogoD) == Este && (fst $ velocidade $ jogador jogoD) /= 0 && (snd $ velocidade $ jogador jogoD) == 0 
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioandar, martelo]
  else if ((direcao $ jogador jogoD) == Este || (direcao $ jogador jogoD) == Norte) && (snd $ velocidade $ jogador jogoD) /= 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) mariosaltar, martelo]
  else if (direcao $ jogador jogoD) == Oeste && (snd $ velocidade $ jogador jogoD) /= 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) mariosaltar, martelo]
  else Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado
  where 
    (px,py) = posicao $ jogador jogoD
    marioparado = fromJust(lookup ("marioparado") images)
    marioandar = 
      if mod' (time state) (1/3) < (1/6)
        then fromJust(lookup ("marioandar1") images)
      else fromJust(lookup ("marioandar2") images)
    marioescada = 
      if mod' (time state) (1/3) < (1/6) && snd (velocidade $ jogador jogoD) /= 0
        then fromJust(lookup ("marioescada") images)
      else scale (-1) (1) $ fromJust(lookup ("marioescada") images)
    mariosaltar = fromJust(lookup ("mariosaltar") images)
    jogoD = jogo state
    martelo = 
     if(fst $ aplicaDano $ jogador jogoD) 
        then 
          if (direcao $ jogador jogoD) == Oeste 
            then if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))-25) (snd(posMapToGloss (px,py)) + 5) $ scale (-0.8) (0.8) $ rotate 45  $ fromJust(lookup ("martelo") images)
                else Translate ((fst(posMapToGloss (px,py)))-25) ((snd(posMapToGloss (px,py))) - 5) $ scale (-0.8) (0.8) $ rotate 90  $ fromJust(lookup ("martelo") images)
          else if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))+25) (snd(posMapToGloss (px,py)) + 5) $ scale (0.8) (0.8) $ rotate 45  $ fromJust(lookup ("martelo") images)
                else Translate ((fst(posMapToGloss (px,py)))+25) ((snd(posMapToGloss (px,py))) - 5) $ scale (0.8) (0.8) $ rotate 90  $ fromJust(lookup ("martelo") images)
      else blank

drawEnemies :: Jogo -> Images -> Picture
drawEnemies jogo images = Pictures $ map (\inimigo -> if direcao inimigo == Este then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ fantasma else Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ scale (-1) (1) fantasma) (inimigos jogo)
  where fantasma = fromJust(lookup ("fantasma") images)

drawColec :: Jogo -> Images -> Picture
drawColec jogo images = Pictures (map (\(colec,pos) -> if colec == Martelo then (Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ martelo) else ( Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ moeda)) (colecionaveis jogo))
  where martelo = fromJust(lookup ("martelo") images)
        moeda = fromJust(lookup ("moeda") images)


drawGame :: State -> Picture
drawGame state = Pictures [drawBlocks (mapa (jogo state)) (images state), drawColec (jogo state) (images state), drawPlayer state (images state),  drawEnemies (jogo state) (images state)]
