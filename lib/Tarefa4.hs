{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Utilities
import GHC.Float (double2Float)
import Mapas (jog)


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo =
  jogo
    { jogador = atualizaJogador (jogador jogo) (mapa jogo) acaoJogador
    , inimigos = zipWith (\inimigo acao -> atualizaInimigo inimigo (mapa jogo) acao) (inimigos jogo) acoesInimigos
    , mapa = mapa jogo
    , colecionaveis = colecionaveis jogo
    }

atualizaJogador ::  Personagem -> Mapa -> Maybe Acao -> Personagem
atualizaJogador jogador _ Nothing = jogador -- Inércia do movimento
atualizaJogador jogador mapa (Just acao) =
  case acao of
    Subir ->
      if inEscada
        then jogador {posicao = ((fromIntegral(floor px) + 0.5),py), velocidade = (0, -3), direcao = Norte}
      else jogador
    Descer ->
      if inEscada || ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox jogador) hitboxbloco) (hitboxesBlocos((mapaPlataformas mapa)))) && (any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+2))) == escadabaixo) (mapaEscadas (mapa))))
        then jogador {posicao = ((fromIntegral(floor px) + 0.5),py), velocidade = (0, 3), direcao = Sul}
      else jogador
    AndarDireita -> 
      if (snd $ velocidade jogador) /= 0 || (inEscada && not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitboxbloco) (hitboxesBlocos(mapaPlataformas(mapa))))) || any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro jogador) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes(blocos)))
        then jogador 
      else jogador {velocidade = (4, snd $ velocidade jogador), direcao = Este}
    AndarEsquerda ->
      if (snd $ velocidade jogador) /= 0  || (inEscada && not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitboxbloco) (hitboxesBlocos(mapaPlataformas(mapa))))) || any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro jogador) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes(blocos)))
        then jogador 
      else jogador {velocidade = (-4, snd $ velocidade jogador), direcao = Oeste}
    Saltar ->
      if (not inEscada || ((any (\primult -> elem (fromIntegral(floor px), fromIntegral(floor py)) primult) (primUltEscadas mapa)) && (fst $ velocidade jogador) /=0)) && any (\hitboxesbloco -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitboxesbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))
        then jogador {velocidade = (fst $ velocidade jogador, -5)}
      else jogador
    Parar -> if emEscada jogador 
              then jogador {velocidade = (0, 0)}
            else jogador {velocidade = (0, snd $ velocidade jogador)}
  where
    (Mapa pidi pf blocos) = mapa
    (px,py) = posicao jogador
    inEscada = emEscada jogador
    tamanhoX = fst $ tamanho jogador
    tamanhoY = snd $ tamanho jogador
    ressaltando = ressalta jogador
    direcaojogador = direcao jogador


atualizaInimigo :: Personagem -> Mapa -> Maybe Acao -> Personagem
atualizaInimigo inimigo _ Nothing = inimigo -- Inércia do movimento
atualizaInimigo inimigo mapa (Just acao) =
  case acao of
    Subir -> 
      if inEscada
        then inimigo {velocidade = (0, -3), direcao = Norte} 
      else inimigo
    Descer -> 
      if inEscada 
        then inimigo {velocidade = (0, 3), direcao = Sul} 
      else inimigo
    AndarDireita -> 
      if not inEscada 
        then inimigo {velocidade = (3, 0), direcao = Este}
        else inimigo
    AndarEsquerda -> 
      if not inEscada 
        then inimigo {velocidade = (-3, 0), direcao = Oeste}
        else inimigo
    Saltar -> 
      if not inEscada
        then inimigo {velocidade = (0, -3)}
      else inimigo
    Parar -> inimigo {velocidade = (0, 0)}
  where inEscada = emEscada inimigo
 
            