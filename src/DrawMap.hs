module DrawMap where

import LI12324
import Utilities
import Graphics.Gloss
import GHC.Float 
import Data.Maybe
import Niveis
import Data.Fixed


windowSize :: (Int,Int)
windowSize = (((length $ head blocos) * float2Int scaleGame),((length blocos) * float2Int scaleGame))
  where (Mapa _ _ blocos) = mapa1

posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (((double2Float x)*scaleGame)-(fromIntegral $ (fst windowSize))/2, ((fromIntegral $ (snd windowSize))/2 - (double2Float y) * scaleGame))

drawBlocks :: State -> Picture
drawBlocks state = Pictures [Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) plataforma) (mapaBlocos mapaD Plataforma),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) alcapao) (mapaBlocos mapaD Alcapao),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) escada) (mapaBlocos mapaD Escada),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) trampolim) (mapaBlocos mapaD Trampolim),
  Pictures $ map (\pos -> Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) lanca) (mapaBlocos mapaD Lanca)]
  where plataforma = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("plataforma") imagesTheme)
        alcapao = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("alcapao") imagesTheme)
        escada = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("escada") imagesTheme)
        lanca = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("spikes") imagesTheme)
        trampolim = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("trampolim") imagesTheme)
        imagesTheme = fromJust (lookup (currentTheme state) (images state))
        mapaD = mapa $ jogo
        jogo = (levelsList state) !! currentLevel state


drawPlayer :: State -> Picture
drawPlayer state= 
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
    jogoD = (levelsList state) !! currentLevel state
    marioparado = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("marioparado") imagesTheme)
    marioandar = 
      if mod' (time state) (1/3) < (1/6)
        then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("marioandar1") imagesTheme)
      else scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("marioandar2") imagesTheme)
    marioescada = 
      if mod' (time state) (1/3) < (1/6) && snd (velocidade $ jogador jogoD) /= 0
        then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("marioescada") imagesTheme)
      else scale (scaleGame/50) (scaleGame/50) $ scale (-1) (1) $ fromJust(lookup ("marioescada") imagesTheme)
    mariosaltar = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("mariosaltar") imagesTheme)
    martelo = 
     if(fst $ aplicaDano $ jogador jogoD) 
        then 
          if (direcao $ jogador jogoD) == Oeste 
            then if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))-23) (snd(posMapToGloss (px,py)) + 5) $ scale (scaleGame/50) (scaleGame/50) $ scale (-0.6) (0.6) $ rotate 45  $ fromJust(lookup ("martelo") imagesTheme)
                else Translate ((fst(posMapToGloss (px,py)))-28) ((snd(posMapToGloss (px,py))) - 5) $ scale (scaleGame/50) (scaleGame/50) $ scale (-0.6) (0.6) $ rotate 90  $ fromJust(lookup ("martelo") imagesTheme)
          else if mod' (time state) (1/2) < (1/4)
                  then Translate ((fst(posMapToGloss (px,py)))+23) (snd(posMapToGloss (px,py)) + 5) $ scale (scaleGame/50) (scaleGame/50) $ scale (0.6) (0.6) $ rotate 45  $ fromJust(lookup ("martelo") imagesTheme)
                else Translate ((fst(posMapToGloss (px,py)))+28) ((snd(posMapToGloss (px,py))) - 5) $ scale (scaleGame/50) (scaleGame/50) $ scale (0.6) (0.6) $ rotate 90  $ fromJust(lookup ("martelo") imagesTheme)
      else blank
    imagesTheme = fromJust (lookup (currentTheme state) (images state))
    imagesThemeDef = fromJust (lookup Mario (images state))


drawEnemies :: State -> Picture
drawEnemies state = Pictures $ map (\inimigo -> if (direcao inimigo == Este ||  direcao inimigo == Norte || direcao inimigo == Sul) && tipo inimigo == Fantasma then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ fantasma else if direcao inimigo == Oeste && tipo inimigo == Fantasma then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ scale (-1) (1) fantasma else if tipo inimigo == MacacoMalvado then Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ macacomalvado else Translate (fst(posMapToGloss (posicao inimigo))) (snd(posMapToGloss (posicao inimigo))) $ barril) (inimigos $ jogo)
  where 
    fantasma =if mod' (time state) (1/4) < (1/8)
      then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("fantasma1") imagesTheme)
    else scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("fantasma2") imagesTheme)
    macacomalvado =if any (\i ->any (\b -> if tipo b== Barril then((fst $ posicao i),(snd $ posicao i)-1.3) == posicao b else False) (inimigos jogo) && tipo i == MacacoMalvado) (inimigos jogo) 
      then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("donkeykong1") imagesTheme)
    else scale (scaleGame/50) (scaleGame/50) $ scale (0.9) (0.9) $ fromJust(lookup ("donkeykong2") imagesTheme)
    barril = 
      if any (\i ->any (\b -> if tipo b== Barril then((fst $ posicao i),(snd $ posicao i)-1.3) == posicao b else False) (inimigos jogo) && tipo i == MacacoMalvado) (inimigos jogo) 
        then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("barril1") imagesTheme)
      else if mod' (time state) (1/4) < (1/8)
             then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("barril1") imagesTheme)
          else scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("barril2") imagesTheme)

    imagesTheme = fromJust (lookup (currentTheme state) (images state))
    jogo = (levelsList state) !! currentLevel state

drawColec :: State -> Picture
drawColec state = Pictures (map (\(colec,pos) -> if colec == Martelo then (Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ martelo) else if colec == Moeda then ( Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ moeda) else ( Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ scale (0.8) (0.8) $ shield)) (colecionaveis jogo))
  where martelo = scale (scaleGame/50) (scaleGame/50)  $ fromJust(lookup ("martelo") imagesTheme)
        moeda = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("moeda") imagesThemeDef)
        shield = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("shield") imagesTheme)
        imagesThemeDef = fromJust (lookup Mario (images state))
        imagesTheme = fromJust (lookup (currentTheme state) (images state))
        jogo = (levelsList state) !! currentLevel state

drawStar :: State -> Picture
drawStar state = Translate (fst(posMapToGloss (posf))) (snd(posMapToGloss (posf))) $ estrela
  where (Mapa (posi,diri) posf blocos) = mapa jogo
        estrela = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("estrela") imagesTheme)
        imagesTheme = fromJust (lookup (currentTheme state) (images state))
        jogo = (levelsList state) !! currentLevel state

drawLife :: Jogo -> Images -> Picture
drawLife jogo images = Pictures (drawHearts (fromIntegral(vida $ jogador jogo))) 
  where 
    drawHearts :: Float -> [Picture]
    drawHearts n 
      | n > 0 =  (Translate ((-380*(scaleGame/50))+(n*50*(scaleGame/50))) (550*(scaleGame/50)) $ heart) : drawHearts (n-1) 
      | otherwise = []
    heart = scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("heart") imagesThemeDef)
    imagesThemeDef = fromJust (lookup Mario images)


drawScore :: State -> Picture
drawScore state = Pictures (drawPoints (currentPoints state))
  where
    drawPoints :: Int -> [Picture]
    drawPoints p = foldl (\pic n -> [Translate ((130*(scaleGame/50)) + (fromIntegral(4-length pic)*50*(scaleGame/50))) (550*(scaleGame/50)) $ scale (scaleGame/50) (scaleGame/50) $ scale (0.8) (0.8) $ (fromJust(lookup ([n]) imagesThemeDef))] ++ pic) [] ps
      where ps = reverse $ show p
    imagesThemeDef = fromJust (lookup Mario (images state))


drawEscuro :: State -> Picture 
drawEscuro state = Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) escuro
  where imagesThemeDef = fromJust (lookup Mario (images state))
        escuro = 
          if currentMode state == Hard 
            then scale (scaleGame/50) (scaleGame/50) $ fromJust(lookup ("escuro") imagesThemeDef)
          else if currentMode state == Medium
            then scale (scaleGame/50) (scaleGame/50) $ scale (2) (2) $ fromJust(lookup ("escuro") imagesThemeDef)
          else blank
        (px,py) = posicao $ jogador jogoD
        jogoD = (levelsList state) !! currentLevel state
    
drawShield :: State -> Picture 
drawShield state = Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) shield
  where imagesThemeDef = fromJust (lookup Mario (images state))
        shield = 
          if fst $ escudo $ jogador jogoD
            then (case (currentTheme state) of
              Mario -> Color blue
              MarioCat -> Color red
              MarioBear -> Color orange
              MarioFrog -> Color green
              MarioAstronaut -> Color yellow) $ scale (scaleGame/50) (scaleGame/50) $ circleSolid (double2Float ty*scaleGame-(0.3*scaleGame)) 
          else blank

        (px,py) = posicao $ jogador jogoD
        (tx,ty) = tamanho $ jogador jogoD
        jogoD = (levelsList state) !! currentLevel state
        blue = makeColorI 0 255 251 150
        red = makeColorI 176 2 2 150
        orange = makeColorI 204 79 2 150
        green = makeColorI 45 255 8 150
        yellow = makeColorI 250 229 0 150


drawGame :: State -> Picture
drawGame state = Pictures [drawBlocks state, drawColec state, drawStar state, drawEnemies state, drawShield state, drawPlayer state,  drawEscuro state,drawLife ((levelsList state) !! currentLevel state) (images state),drawScore state]
