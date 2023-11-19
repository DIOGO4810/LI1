{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

-- | Função para verificar colisões com plataformas e os limites do Mapa laterais e superior
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa (_, _) (limiteX, limiteY) matriz) personagem = colide
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem
    -- | Verifica se o personagem está fora dos limites laterais
    foraDosLimitesLaterais = px - tamanhoX / 2 < 0 || px + tamanhoX / 2 > limiteX
    -- | Verifica se o personagem está abaixo do limite superior (não colide com o topo)
    abaixoDoLimiteSuperior = py + tamanhoY / 2 < limiteY
    -- | Verifica se o personagem está em algum bloco de plataforma investigando os elementos verticais e horizontais da matriz
    emPlataforma i j = case matriz !! i !! j of
      Plataforma -> True
      _ -> False
    -- | Verifica se o personagem colide com a plataforma usando a função floor para combinar os valores reais recebidos com os inteiros da matriz
    colideComPlataforma l = any (\i -> any (\j -> emPlataforma i j) [floor px, floor (px + tamanhoX)]) l
    -- | Combinando as condições na função principal
    colide = foraDosLimitesLaterais || (abaixoDoLimiteSuperior && (colideComPlataforma [floor px, floor (px + tamanhoX)]))






colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colide
  where
    hitboxP1 = calculaHitbox p1
    hitboxP2 = calculaHitbox p2
    colide = colisaoHitbox hitboxP1 hitboxP2

-- | Função auxiliar para calcular a hitbox de um personagem considerando que px e py estão no respetivo centro
calculaHitbox :: Personagem -> Hitbox
calculaHitbox personagem =
  ((px - tamanhoX / 2, py - tamanhoY / 2), (px + tamanhoX / 2, py + tamanhoY / 2))
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem

-- | Função auxiliar para verificar colisão entre duas hitboxes
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3
