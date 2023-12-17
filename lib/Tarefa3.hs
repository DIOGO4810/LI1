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
    , inimigos = isInimigoDead $ tiraVidaInimigos $ map (\i -> colideEscada $ stopLimites $ aplicaGravidade i) listaInimigos
    , colecionaveis = tiraColecionaveis listaColecionaveis
    , jogador = moveJogador $ colideEscada $ tiraVidaJogador $ aplicaEfeitos $ stopLimites $ ejeta $ aplicaGravidade jogadorJogo
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
        then  map (\personagem -> if colinimigo personagem then (personagem{vida=vida personagem-1}) else personagem) (listaInimigos)
      else listaInimigos 
      where 
        colide = any colinimigo (listaInimigos) 
        colinimigo inimigo = colisaoHitbox (calculaHitboxDano jogadorJogo) (calculaHitbox inimigo)

    -- | 2. Função que faz os inimigos desaparecer do mapa quando perdem todas as vidas
    isInimigoDead :: [Personagem] -> [Personagem]
    isInimigoDead listaInimigos =
      if isDead
        then map (\personagem -> if vida personagem == 0 then personagem {vida=0, posicao = (-100,-100)} else personagem) listaInimigos
      else listaInimigos
      where isDead = any (\personagem -> vida personagem==0) listaInimigos

    -- | 3. Função que aplica um efeito de gravidade aos personagens se eles não se encontrarem numa plataforma ou num alçapão
    aplicaGravidade :: Personagem -> Personagem
    aplicaGravidade personagem =
      if (emEscada personagem == False) && (colideComBloco personagem (mapa jogo)) && (snd $ velocidade personagem)>=0 
        then personagem {velocidade=(fst $ velocidade personagem,0)}
      else if emEscada personagem == False
        then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
      else if emEscada personagem && (fst $ velocidade personagem) /= 0 && not (any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas (mapa jogo))))
        then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
      else personagem {velocidade=(fst $ velocidade personagem, snd $ velocidade personagem)}

    -- | 4. Função que retira uma vida ao jogador quando ele colide com um inimigo e está desarmado
    tiraVidaJogador :: Personagem -> Personagem
    tiraVidaJogador personagem = 
      if armado == False && colide 
        then personagem{vida=vida personagem-1} 
      else personagem{vida=vida personagem}
      where
        colide = any colinimigo listaInimigos
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
    aplicaEfeitos personagem =
      if length col > 0
        then case fst(head col) of
              Moeda -> personagem {pontos=pontos personagem+100}
              Martelo -> personagem {aplicaDano=(True,10)}
      else personagem
      where col = filter (\(col,(x,y)) -> colisaoHitbox (calculaHitbox jogadorJogo) ((x-0.5,y+0.5),(x+0.5,y-0.5))) listaColecionaveis

    -- | 6. Função que faz um alcapão desaparecer depois do jogador o pisar
    tiraAlcapoes :: Mapa -> Mapa
    tiraAlcapoes (Mapa ((xi,yi),dir) (xf,yf) blocos) =
      if (any (\hitboxalcapao -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxalcapao) hitboxesalcapoes) && (getBloco (px+0.5,py+1) blocos == Alcapao) && (direcao jogadorJogo) == Oeste
        then (Mapa ((xi,yi),dir) (xf,yf) (replace blocos (px+0.5,py)))
      else if (any (\hitboxalcapao -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxalcapao) hitboxesalcapoes) && (getBloco (px-0.5,py+1) blocos == Alcapao) && (direcao jogadorJogo) == Este
        then (Mapa ((xi,yi),dir) (xf,yf) (replace blocos (px-0.5,py)))
      else (Mapa ((xi,yi),dir) (xf,yf) blocos)
      where 
        colide = any (\hitboxbloco -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxbloco) hitboxesalcapoes
        posalcapoes = mapaAlcapoes (Mapa ((xi,yi),dir) (xf,yf) blocos)
        hitboxesalcapoes = hitboxesBlocos posalcapoes

    -- | 7. Função que impede os personagens de sair dos limites do mapa e atravessar plataformas
    stopLimites ::Personagem -> Personagem
    stopLimites personagem = 
      if (((px + tamanhoX/2) >= fromIntegral(length (head blocos))) || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) > 0 
        then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Oeste}
      else if ((px - tamanhoX/2) <= 0 || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) < 0 
        then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Este}
      else if emEscada personagem && any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas (mapa jogo))) && not (any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+1))) == escadabaixo) (mapaEscadas (mapa jogo))) && (snd $ velocidade personagem)>0
        then personagem {velocidade = (0,0)}
      else if py - tamanhoY / 2 <= 0
        then personagem {velocidade = (fst $ velocidade personagem,10)}
      else personagem

    -- | Função que verifica se um personagem está numa escada
    colideEscada :: Personagem -> Personagem
    colideEscada personagem= 
      if any (\(xe,ye) -> fromIntegral(floor $ fst $ posicao personagem) == xe && fromIntegral(floor $ snd $ posicao personagem) == ye) (mapaEscadas (mapa jogo))
        then personagem{emEscada=True}
      else personagem {emEscada = False}

    -- | Função que o faz o jogador mover-se
    moveJogador :: Personagem -> Personagem
    moveJogador personagem = 
      personagem {posicao = (px+(fst $ velocidade personagem)*tempo,py+(snd $ velocidade personagem)*tempo)}

    -- | Função que ejeta o jogador se ele ficar preso num bloco
    ejeta :: Personagem -> Personagem
    ejeta personagem = 
      if any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas(mapa jogo)))
        then personagem {velocidade = (fst $ (velocidade personagem), -3)}
      else personagem
