{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
  
import Data.Maybe
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float, int2Double)

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo = 
  Jogo 
    { mapa = tiraAlcapoes (mapa jogo)
    , inimigos = isInimigoDead $ tiraVidaInimigos $ map aplicaGravidade listaInimigos
    , colecionaveis = tiraColecionaveis listaColecionaveis
    , jogador = tiraVidaJogador $ aplicaEfeitos $ aplicaGravidade jogadorJogo
    }

  where 
    (Mapa ((xi,yi),dir) (xf,yf) blocos) = mapa jogo
    listaInimigos = inimigos jogo
    jogadorJogo = jogador jogo
    listaColecionaveis = colecionaveis jogo
    (px,py) = posicao jogadorJogo
    (tamanhoX, tamanhoY) = tamanho jogadorJogo
    (armado, tempoarmado) = aplicaDano jogadorJogo

    -- | 1. Função que retira 1 vida aos inimigos se eles colidirem com a hitbox de dano de um jogador armado
    tiraVidaInimigos :: [Personagem] -> [Personagem]
    tiraVidaInimigos listaInimigos = 
      if armado == True && tempoarmado > 0 && colide
        then  map (\(Personagem{vida=v}) -> if colinimigo (Personagem{vida=v}) then (Personagem{vida=v-1}) else (Personagem{vida=v}) ) (listaInimigos)
      else listaInimigos 
      where 
        colide = any colinimigo (listaInimigos) 
        colinimigo inimigo = colisaoHitbox (calculaHitboxDano jogadorJogo) (calculaHitbox inimigo)

    -- | 2. Função que faz os inimigos desaparecer do mapa quando perdem todas as vidas
    isInimigoDead :: [Personagem] -> [Personagem]
    isInimigoDead listaInimigos =
      if isDead
        then map (\(Personagem {vida=v, posicao = (x,y)}) -> if v == 0 then Personagem {vida=0, posicao = (-100,-100)} else Personagem {vida=v, posicao = (x,y)}) listaInimigos
      else listaInimigos
      where isDead = any (\Personagem{vida=v} -> v==0) listaInimigos

    -- | 3. Função que aplica um efeito de gravidade aos personagens se eles não se encontrarem numa plataforma ou num alçapão
    aplicaGravidade :: Personagem -> Personagem
    aplicaGravidade Personagem{velocidade=(vx,vy)}=
      if colideComBloco Personagem{velocidade=(vx,vy)} (mapa jogo)
        then Personagem{velocidade=gravidade}
      else Personagem{velocidade=(vx,vy)}

    -- | 4. Função que retira uma vida ao jogador quando ele colide com um inimigo e está desarmado
    tiraVidaJogador :: Personagem -> Personagem
    tiraVidaJogador Personagem{vida=v} = 
      if armado == False && colide 
        then Personagem{vida=v-1} 
      else Personagem{vida=v}
      where
        colide = any colinimigo (listaInimigos) 
        colinimigo inimigo = colisaoHitbox (calculaHitbox jogadorJogo) (calculaHitbox inimigo)
    
    -- | 5. Função que retira os colecionáveis quando são coletados e aplica os seus efeitos
    tiraColecionaveis :: [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
    tiraColecionaveis listaColecionaveis = 
      if colide 
        then filter (\(col,(x,y)) -> not (colisaoHitbox (calculaHitbox jogadorJogo) ((x-0.5,y+0.5),(x+0.5,y-0.5)))) listaColecionaveis
      else listaColecionaveis
      where 
        colide = any (\hitboxescolecionavel -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxescolecionavel) hitboxescolecionaveis
        hitboxescolecionaveis = hitboxesColecionaveis listaColecionaveis

    aplicaEfeitos :: Personagem -> Personagem
    aplicaEfeitos Personagem {pontos=p,aplicaDano=(a,ta)} =
      case cos of
        Moeda -> Personagem {pontos=p+100,aplicaDano=(a,ta)}
        Martelo -> Personagem {pontos=p,aplicaDano=(True,10)}
      where [(cos,pos)] = filter (\(col,(x,y)) -> colisaoHitbox (calculaHitbox jogadorJogo) ((x-0.5,y+0.5),(x+0.5,y-0.5))) listaColecionaveis

    -- | 6. Função que faz um alcapão desaparecer depois do jogador o pisar
    tiraAlcapoes :: Mapa -> Mapa
    tiraAlcapoes (Mapa ((xi,yi),dir) (xf,yf) blocos) =
      if colide && (elem (int2Double(floor px), int2Double(ceiling py)) posalcapoes)
        then (Mapa ((xi,yi),dir) (xf,yf) (replace blocos (px,py)))
      else (Mapa ((xi,yi),dir) (xf,yf) blocos)
      where 
        colide = any (\hitboxbloco -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxbloco) hitboxesalcapoes
        posalcapoes = mapaAlcapoes (Mapa ((xi,yi),dir) (xf,yf) blocos)
        hitboxesalcapoes = hitboxesBlocos posalcapoes

    -- | 7. Função que impede os personagens de sair dos limites do mapa e atravessar plataformas
    

-- | Funções auxiliares

calculaHitboxDano :: Personagem -> Hitbox
calculaHitboxDano jogador = 
  case dir of 
    Este -> (((px+tx/2),(py+ty/2)),((px+tx*1.5),(py-ty/2)))
    Oeste -> (((px-tx*1.5),(py+ty/2)),((px-tx/2),(py-ty/2)))
    Norte -> (((px-tx/2),(py+ty*1.5)),((px+tx/2),(py+ty/2)))
    Sul -> (((px-tx/2),(py-ty/2)),((px+tx/2),(py-ty*1.5)))
  where 
    (px,py) = posicao jogador
    dir = direcao jogador
    (tx,ty) = tamanho jogador

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

-- | Verifica se o personagem colide com plataformas ou alçapões
colideComBloco :: Personagem -> Mapa -> Bool
colideComBloco personagem (Mapa _ _ blocos) = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))

-- | Verifica se o personagem colide com alçapões
colideComAlcapoes :: Personagem -> Mapa -> Bool
colideComAlcapoes personagem (Mapa _ _ blocos) = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))

-- | Troca um bloco pelo vazio
replace :: [[Bloco]] -> Posicao -> [[Bloco]]
replace blocos (px, py) = takeWhile (/= linhapy) blocos ++ [replaceVazio (head $ dropWhile (/= linhapy) blocos) (floor px)] ++ drop 1 (dropWhile (/= linhapy) blocos)
  where
    linhapy = blocos !! ceiling py

replaceVazio :: [Bloco] -> Int -> [Bloco]
replaceVazio [] _ = []
replaceVazio (x:xs) 0 = Vazio : xs
replaceVazio (x:xs) n = x : replaceVazio xs (n - 1)

-- | Função que retorna uma lista com as hitboxes dos colecionaveis
hitboxesColecionaveis :: [(Colecionavel,Posicao)] -> [Hitbox]
hitboxesColecionaveis = map (\(col,(x,y)) -> ((x-0.5,y+0.5),(x+0.5,y-0.5))) 

-- | Função que retorna uma lista com as hitboxes dos blocos de plataforma e alçapão
hitboxesBlocos :: [Posicao] -> [Hitbox]
hitboxesBlocos = map (\(x,y) -> ((x,y),(x+1,y+1))) 

-- | Função que retorna a lista de posições das plataformas e dos alçapões
mapaPlataformasAlcapoes :: [[Bloco]] -> [Posicao]
mapaPlataformasAlcapoes blocos = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos) || isPlataforma (getBloco pos blocos)]

mapaAlcapoes :: Mapa -> [Posicao]
mapaAlcapoes (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos)]

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