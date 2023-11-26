{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float)

jogador1 = Personagem {posicao = (14.6,1), tamanho=(1,1)}

-- | Função para verificar colisões com plataformas e os limites do Mapa laterais e superior
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) personagem = foraDosLimitesLaterais || acimaDoLimiteSuperior || colideComBloco
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem
    -- | Verifica se o personagem colide ou está fora dos limites laterais
    foraDosLimitesLaterais = px - tamanhoX / 2 <= 0 || px + tamanhoX / 2 >= fromIntegral (length(head blocos))
    -- | Verifica se o personagem colide ou está fora do limite superior
    acimaDoLimiteSuperior = py - tamanhoY / 2 <= 0
    -- | Verifica se o personagem colide com plataformas ou alçapões
    colideComBloco = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))


colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoHitbox hitboxP1 hitboxP2
  where
    hitboxP1 = calculaHitbox p1
    hitboxP2 = calculaHitbox p2

-- | Funções auxiliares

-- | Função para calcular a hitbox de um personagem considerando que px e py estão no respetivo centro
calculaHitbox :: Personagem -> Hitbox
calculaHitbox personagem =
  ((px - tamanhoX / 2, py + tamanhoY / 2), (px + tamanhoX / 2, py - tamanhoY / 2))
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem

-- | Função auxiliar para verificar a colisão entre duas hitboxes
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox h1 h2 = colisaoHitboxAux h1 h2 || colisaoHitboxAux h2 h1

colisaoHitboxAux :: Hitbox -> Hitbox -> Bool
colisaoHitboxAux ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) = pointInBox (double2Float x3,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x3,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)

-- | Função que retorna uma lista com as hitboxes dos blocos de plataforma e alçapão
hitboxesBlocos :: [Posicao] -> [Hitbox]
hitboxesBlocos = map (\(x,y) -> ((x,y),(x+1,y+1))) 

-- | Função que retorna a lista de posições das plataformas e dos alçapões
mapaPlataformasAlcapoes :: [[Bloco]] -> [Posicao]
mapaPlataformasAlcapoes blocos = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos) || isPlataforma (getBloco pos blocos)]

indicesBlocos :: [[Bloco]] -> [Posicao]
indicesBlocos blocos = [(fromIntegral j, fromIntegral i) | i<- [0..height-1], j <- [0..width-1]]
  where
    height = length blocos
    width = length (head blocos)

getBloco :: Posicao -> [[Bloco]] -> Bloco
getBloco (i, j) blocos = (blocos !! floor j) !! floor i

isAlcapao :: Bloco -> Bool
isAlcapao Alcapao = True
isAlcapao _ = False

isPlataforma :: Bloco -> Bool
isPlataforma Plataforma = True
isPlataforma _ = False