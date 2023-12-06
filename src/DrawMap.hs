module DrawMap where

import LI12324
import Utilities
import Graphics.Gloss
import GHC.Float (double2Float)
import Mapas

scaleGame :: Int
scaleGame = 50

windowSize :: (Int,Int)
windowSize = (((length $ head blocos) * scaleGame),((length blocos) * scaleGame))
  where (Mapa _ _ blocos) = mapa1

posMapToGloss :: Posicao -> (Float,Float)
posMapToGloss (x,y) = (((double2Float x)*fromIntegral scaleGame)-(fromIntegral $(fst windowSize))/2, ((fromIntegral $ (snd windowSize))/2 - (double2Float y) * fromIntegral scaleGame))

drawBlocks :: Mapa -> Picture -> [Picture]
drawBlocks mapa img = map (\pos -> Color red $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ rectangleSolid 50 50) (mapaBlocos mapa Plataforma) ++ map (\pos -> Color blue $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ rectangleSolid 50 50) (mapaBlocos mapa Alcapao) ++ map (\pos -> Color orange $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ rectangleSolid 50 50) (mapaBlocos mapa Escada) 

drawColec :: Jogo -> Picture -> [Picture]
drawColec jogo img = map (\(colec,pos) -> if colec == Martelo then (Color green $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ circleSolid 25) else (Color yellow $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ circleSolid 25)) (colecionaveis jogo)

drawGame :: Jogo -> Picture
drawGame jogo = Pictures (drawBlocks (mapa jogo) blank ++ drawColec jogo blank)

