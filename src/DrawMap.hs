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

drawBlocks :: Mapa -> [(String,Picture)] -> [Picture]
drawBlocks mapa images = map (\pos -> Color red $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ plataforma) (mapaBlocos mapa Plataforma) ++ map (\pos -> Color blue $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ alcapao) (mapaBlocos mapa Alcapao) ++ map (\pos -> Color orange $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ escada) (mapaBlocos mapa Escada) 
  where plataforma = fromJust(lookup ("plataforma") images)
        alcapao = fromJust(lookup ("alcapao") images)
        escada = fromJust(lookup ("escada") images)



drawPlayer :: Jogo -> [(String,Picture)] -> Picture
drawPlayer jogo images = Color white $ Translate (fst(posMapToGloss (px,py))) (snd(posMapToGloss (px,py))) $ mario
  where 
    (px,py) = posicao $ jogador jogo
    mario = fromJust(lookup ("mario") images)

drawColec :: Jogo -> [(String,Picture)] -> [Picture]
drawColec jogo images = map (\(colec,pos) -> if colec == Martelo then (Color green $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ martelo) else (Color yellow $ Translate (fst(posMapToGloss pos)) (snd(posMapToGloss pos)) $ moeda)) (colecionaveis jogo)
  where martelo = fromJust(lookup ("martelo") images)
        moeda = fromJust(lookup ("moeda") images)


drawGame :: Jogo -> [(String,Picture)] -> Picture
drawGame jogo images= Pictures (drawBlocks (mapa jogo) images ++ drawColec jogo images ++ [drawPlayer jogo images])

