module DrawMap where

import LI12324
import Utilities
import Graphics.Gloss
import GHC.Float (double2Float, int2Float)
import Data.Maybe
import Niveis
import Data.Fixed


scaleGame :: Int
scaleGame = 50

windowSize :: (Int,Int)
windowSize = (((length $ head blocos) * scaleGame),((length blocos) * scaleGame))
  where (Mapa _ _ blocos) = mapa1

posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (((double2Float x)*fromIntegral scaleGame)-(fromIntegral $ (fst windowSize))/2, ((fromIntegral $ (snd windowSize))/2 - (double2Float y) * fromIntegral scaleGame))

drawBlocks :: State -> Picture
drawBlocks state = Pictures [Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ plataforma) (mapaBlocos mapaD Plataforma),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) alcapao) (mapaBlocos mapaD Alcapao),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) escada) (mapaBlocos mapaD Escada)]
  where plataforma = fromJust(lookup ("plataforma") imagesTheme)
        alcapao = fromJust(lookup ("alcapao") imagesTheme)
        escada = fromJust(lookup ("escada") imagesTheme)
        imagesTheme = fromJust (lookup (currentTheme state) (images state))
        mapaD = mapa $ jogo
        jogo = (levelsList state) !! currentLevel state


drawPlayer :: State -> Images -> Picture
drawPlayer state images = 
  if ((direcao $ jogador jogoD) == Norte  || (direcao $ jogador jogoD) == Sul) && (emEscada $ jogador jogoD)
    then Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioescada
  else if (direcao $ jogador jogoD) == Oeste && (fst $ velocidade $ jogador jogoD) == 0 && (snd $ velocidade $ jogador jogoD) == 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioparado, martelo]
  else if ((direcao $ jogador jogoD) == Este || (direcao $ jogador jogoD) == Norte) && (fst $ velocidade $ jogador jogoD) == 0 && (snd $ velocidade $ jogador jogoD) == 0 
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado, martelo]
  else if (direcao $ jogador jogoD) == Oeste && (fst $ velocidade $ jogador jogoD) /= 0 && (snd $ velocidade $ jogador jogoD) == 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) marioandar, martelo]
  else if (direcao $ jogador jogoD) == Este && (fst $ velocidade $ jogador jogoD) /= 0 && (snd $ velocidade $ jogador jogoD) == 0 
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioandar, martelo]
  else if ((direcao $ jogador jogoD) == Este || (direcao $ jogador jogoD) == Norte) && (snd $ velocidade $ jogador jogoD) /= 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) mariosaltar, martelo]
  else if (direcao $ jogador jogoD) == Oeste && (snd $ velocidade $ jogador jogoD) /= 0
    then Pictures[Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ scale (-1) (1) mariosaltar, martelo]
  else Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) marioparado
  where 
    (px,py) = posicao $ jogador jogoD
    marioparado = fromJust(lookup ("marioparado") imagesTheme)
    marioandar = 
      if mod' (time state) (1/3) < (1/6)
        then fromJust(lookup ("marioandar1") imagesTheme)
      else fromJust(lookup ("marioandar2") imagesTheme)
    marioescada = 
      if mod' (time state) (1/3) < (1/6) && snd (velocidade $ jogador jogoD) /= 0
        then fromJust(lookup ("marioescada") imagesTheme)
      else scale (-1) (1) $ fromJust(lookup ("marioescada") imagesTheme)
    mariosaltar = fromJust(lookup ("mariosaltar") imagesTheme)
    jogoD = (levelsList state) !! currentLevel state
    martelo = 
     if(fst $ aplicaDano $ jogador jogoD) 
        then 
          if (direcao $ jogador jogoD) == Oeste 
            then if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))-23) (snd(posMapToGloss (px,py)) + 5) $ scale (-0.8) (0.8) $ rotate 45  $ fromJust(lookup ("martelo") imagesThemeDef)
                else Translate ((fst(posMapToGloss (px,py)))-28) ((snd(posMapToGloss (px,py))) - 5) $ scale (-0.8) (0.8) $ rotate 90  $ fromJust(lookup ("martelo") imagesThemeDef)
          else if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))+23) (snd(posMapToGloss (px,py)) + 5) $ scale (0.8) (0.8) $ rotate 45  $ fromJust(lookup ("martelo") imagesThemeDef)
                else Translate ((fst(posMapToGloss (px,py)))+28) ((snd(posMapToGloss (px,py))) - 5) $ scale (0.8) (0.8) $ rotate 90  $ fromJust(lookup ("martelo") imagesThemeDef)
      else blank
    imagesTheme = fromJust (lookup (currentTheme state) images)
    imagesThemeDef = fromJust (lookup Mario images)


drawEnemies :: State -> Images -> Picture
drawEnemies state images = Pictures $ map (\inimigo -> if direcao inimigo == Este then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ fantasma else Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ scale (-1) (1) fantasma) (inimigos $ jogo)
  where 
    fantasma =if mod' (time state) (1/4) < (1/8)
      then fromJust(lookup ("fantasma1") imagesTheme)
    else fromJust(lookup ("fantasma2") imagesTheme)
    imagesTheme = fromJust (lookup (currentTheme state) images)
    jogo = (levelsList state) !! currentLevel state

drawColec :: State -> Picture
drawColec state = Pictures (map (\(colec,pos) -> if colec == Martelo then (Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ martelo) else ( Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ moeda)) (colecionaveis jogo))
  where martelo = fromJust(lookup ("martelo") imagesThemeDef)
        moeda = fromJust(lookup ("moeda") imagesThemeDef)
        imagesThemeDef = fromJust (lookup Mario (images state))
        jogo = (levelsList state) !! currentLevel state

drawStar :: State -> Picture
drawStar state = Translate (fst(posMapToGloss (posf))) (snd(posMapToGloss (posf))) $ estrela
  where (Mapa (posi,diri) posf blocos) = mapa $ jogo
        estrela = fromJust(lookup ("estrela") imagesTheme)
        imagesTheme = fromJust (lookup (currentTheme state) (images state))
        jogo = (levelsList state) !! currentLevel state

drawLife :: Jogo -> Images -> Picture
drawLife jogo images = Pictures (drawHearts (fromIntegral(vida $ jogador jogo))) 
  where 
    drawHearts :: Float -> [Picture]
    drawHearts n 
      | n > 0 =  (Translate (-380+(n*50)) 550 $ heart) : drawHearts (n-1) 
      | otherwise = []
    heart = fromJust(lookup ("heart") imagesThemeDef)
    imagesThemeDef = fromJust (lookup Mario images)


drawScore :: Jogo -> Images -> Picture
drawScore jogo images = Pictures (drawPoints (fromIntegral(pontos $ jogador jogo)))
  where
    drawPoints :: Int -> [Picture]
    drawPoints p = foldl (\pic n -> [Translate (130 + (fromIntegral(4-length pic)*50)) 550 $ scale (0.8) (0.8) $ (fromJust(lookup ([n]) imagesThemeDef))] ++ pic) [] ps
      where ps = reverse $ show p
    imagesThemeDef = fromJust (lookup Mario images)


drawGame :: State -> Picture
drawGame state = Pictures [drawBlocks state, drawColec state, drawStar state, drawPlayer state (images state),  drawEnemies state (images state),drawLife ((levelsList state) !! currentLevel state) (images state),drawScore ((levelsList state) !! currentLevel state) (images state)]
