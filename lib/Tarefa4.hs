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


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo =
  Jogo
    { jogador = atualizaJogador (jogador jogo) (mapa jogo) acaoJogador
    , inimigos = zipWith (\inimigo acao -> atualizaInimigo inimigo (mapa jogo) acao) (inimigos jogo) acoesInimigos
    , mapa = mapa jogo
    , colecionaveis = colecionaveis jogo
    }

atualizaJogador ::  Personagem-> Mapa -> Maybe Acao -> Personagem
atualizaJogador jogador (Mapa _ _ blocos) Nothing = jogador -- Inércia do movimento
atualizaJogador jogador (Mapa _ _ blocos) (Just acao) =
  case acao of
    Subir ->
      if inEscada 
        then jogador {velocidade = (0, -2), direcao = Norte}
      else jogador
    Descer ->
      if inEscada
        then jogador {velocidade = (0, 2), direcao = Sul}
      else jogador
    AndarDireita ->
      if not inEscada && (px + tamanhoX/2) < fromIntegral(length (head blocos)) && not ((any (\hitboxbloco -> colisaoHitbox (calculaHitboxObstaculo jogador) hitboxbloco) (hitboxesBlocos $ mapaPlataformasAlcapoes blocos))&& direcaojogador == Este)
        then jogador {velocidade = (5, 0), direcao = Este}
      else jogador
    AndarEsquerda -> 
      if not inEscada && (px + tamanhoX/2) < fromIntegral (length (head blocos)) && not ((any (\hitboxbloco -> colisaoHitbox (calculaHitboxObstaculo jogador) hitboxbloco) (hitboxesBlocos $ mapaPlataformasAlcapoes blocos))&& direcaojogador == Oeste)
        then jogador {velocidade = (-5, 0), direcao = Oeste }
      else jogador
    Saltar ->
      if not inEscada
        then jogador {velocidade = (0, -1)}
      else jogador
    Parar -> jogador {velocidade = (0, 0)}
  where
    (px,py) = posicao jogador
    inEscada = emEscada jogador
    tamanhoX = fst $ tamanho jogador
    tamanhoY = snd $ tamanho jogador
    ressaltando = ressalta jogador
    direcaojogador = direcao jogador


atualizaInimigo :: Personagem -> Mapa -> Maybe Acao -> Personagem
atualizaInimigo inimigo (Mapa ((x,y),_) _ blocos) Nothing = inimigo -- Inércia do movimento
atualizaInimigo inimigo (Mapa ((x,y),_) _ blocos) (Just acao) =
  case acao of
    Subir -> 
      if inEscada
        then inimigo { velocidade = (0, -1), direcao = Norte} 
      else inimigo
    Descer -> 
      if inEscada 
        then inimigo { velocidade = (0, 1), direcao = Sul} 
      else inimigo
    AndarDireita -> 
      if not inEscada && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head blocos)))-0.001) 
        then inimigo {velocidade = (1, 0), direcao = Este}
        else inimigo
    AndarEsquerda -> 
      if not inEscada && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head blocos)))-0.001)
        then inimigo {velocidade = (-1, 0), direcao = Oeste }
        else inimigo
    Saltar -> 
      if not inEscada
        then inimigo {velocidade = (0, -1)}
      else inimigo
    Parar -> inimigo { velocidade = (0, 0) }
  where
    (px,py) = posicao inimigo
    inEscada = emEscada inimigo
    tamanhoX = fst $ tamanho inimigo
    tamanhoY = snd $ tamanho inimigo
    ressaltando = ressalta inimigo
 
            