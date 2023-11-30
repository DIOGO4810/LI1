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

