{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Utilities
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float)

jogador1 = Personagem {posicao = (14.6,1), tamanho=(1,1)}

-- | Função para verificar colisões com plataformas e os limites do Mapa laterais e superior
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) personagem = colide
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem
    -- | Verifica se o personagem colide ou está fora dos limites laterais
    foraDosLimitesLaterais = px - tamanhoX / 2 <= 0 || px + tamanhoX / 2 >= fromIntegral (length(head blocos))
    -- | Verifica se o personagem colide ou está fora do limite superior
    acimaDoLimiteSuperior = py - tamanhoY / 2 <= 0
    -- | Verifica se o personagem colide com plataformas ou alçapões
    colideComBloco = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesPlataformasAlcapoes(mapaPlataformasAlcapoes blocos))
    -- | Combinando as condições na função principal
    colide = foraDosLimitesLaterais || acimaDoLimiteSuperior || colideComBloco


colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colide
  where
    hitboxP1 = calculaHitbox p1
    hitboxP2 = calculaHitbox p2
    colide = colisaoHitbox hitboxP1 hitboxP2

