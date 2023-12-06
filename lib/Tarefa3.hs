{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Utilities
import Data.Maybe
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float, int2Double)

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo = 
  Jogo 
    { mapa = tiraAlcapoes (mapa jogo)
    , inimigos = isInimigoDead $ tiraVidaInimigos $ map (\i -> stopLimites $ aplicaGravidade i) listaInimigos
    , colecionaveis = tiraColecionaveis listaColecionaveis
    , jogador = tiraVidaJogador $ aplicaEfeitos $ stopLimites $ aplicaGravidade jogadorJogo
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
      if not (colideComBloco Personagem{velocidade=(vx,vy)} (mapa jogo))
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
    stopLimites ::Personagem -> Personagem
    stopLimites Personagem {velocidade = (vx,vy),direcao = dir, ressalta = res} =
      if (px - tamanhoX / 2 <= 0 || px + tamanhoX / 2 >= fromIntegral (length(head blocos)) || any (\h1-> colisaoHitbox h1 (calculaHitboxObstaculo Personagem {velocidade = (vx,vy),direcao = dir, ressalta = res})) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && res == False
        then Personagem {velocidade = (0,vy), direcao = dir, ressalta= False}
      else if (px - tamanhoX / 2 <= 0 || px + tamanhoX / 2 >= fromIntegral (length(head blocos)) || any (\h1-> colisaoHitbox h1 (calculaHitboxObstaculo Personagem {velocidade = (vx,vy),direcao = dir, ressalta = res})) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && res == True
        then Personagem {velocidade = (-vx,vy), direcao = if dir == Este then Oeste else Este, ressalta= True}
      else if py - tamanhoY / 2 <= 0
        then Personagem {velocidade = (vx,10),direcao = dir, ressalta = res}
      else Personagem {velocidade = (vx,vy),direcao = dir, ressalta = res} 



