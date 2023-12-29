module DrawMap where

import LI12324
import Utilities
import Graphics.Gloss
import GHC.Float (double2Float)
import Data.Maybe
import Mapas


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

drawPlayer :: Jogo -> Images -> Picture
drawPlayer jogo images = 
  if (direcao $ jogador jogo) == Oeste && (fst $ velocidade $ jogador jogo) == 0 && (snd $ velocidade $ jogador jogo) == 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioparado
  else if (direcao $ jogador jogo) == Este && (fst $ velocidade $ jogador jogo) == 0 && (snd $ velocidade $ jogador jogo) == 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado
  else if (direcao $ jogador jogo) == Oeste && (fst $ velocidade $ jogador jogo) /= 0 && (snd $ velocidade $ jogador jogo) == 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioandar
  else if (direcao $ jogador jogo) == Este && (fst $ velocidade $ jogador jogo) /= 0 && (snd $ velocidade $ jogador jogo) == 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioandar
  else if ((direcao $ jogador jogo) == Norte  || (direcao $ jogador jogo) == Sul) && (emEscada $ jogador jogo)
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioescada
  else if (direcao $ jogador jogo) == Este && (snd $ velocidade $ jogador jogo) /= 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) mariosaltar
  else if (direcao $ jogador jogo) == Oeste && (snd $ velocidade $ jogador jogo) /= 0
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) mariosaltar
  else Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado
  where 
    (px,py) = posicao $ jogador jogo
    marioparado = fromJust(lookup ("marioparado") images)
    marioandar = fromJust(lookup ("marioandar") images)
    marioescada = fromJust(lookup ("marioescada") images)
    mariosaltar = fromJust(lookup ("mariosaltar") images)

drawEnemies :: Jogo -> Images -> Picture
drawEnemies jogo images = Pictures $ map (\inimigo -> if direcao inimigo == Este then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ fantasma else Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ scale (-1) (1) fantasma) (inimigos jogo)
  where fantasma = fromJust(lookup ("fantasma") images)

drawColec :: Jogo -> Images -> Picture
drawColec jogo images = Pictures (map (\(colec,pos) -> if colec == Martelo then (Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ martelo) else ( Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ moeda)) (colecionaveis jogo))
  where martelo = fromJust(lookup ("martelo") images)
        moeda = fromJust(lookup ("moeda") images)


drawGame :: State -> Picture
drawGame state = Pictures [drawBlocks (mapa (jogo state)) (images state), drawColec (jogo state) (images state), drawPlayer (jogo state) (images state),  drawEnemies (jogo state) (images state)]
