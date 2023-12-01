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
atualizaJogador jogador (Mapa ((x, y), _) _ matriz) Nothing = jogador -- Inércia do movimento
atualizaJogador jogador (Mapa ((x, y), d) _ matriz) (Just acao) =
  case acao of
    Subir ->
      if emEscad y x matriz
        then jogador {velocidade = (0, -1), direcao = Norte}
        else jogador
    Descer ->
      if emEscad y x matriz
        then jogador {velocidade = (0, 1), direcao = Sul}
        else jogador
    AndarDireita ->
      if (not (tlvsubirEdescer jogador)) && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head matriz)))-0.001)
        then jogador {velocidade = (1, 0), direcao = Este}
        else jogador
    AndarEsquerda -> 
      if (not (tlvsubirEdescer jogador)) && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head matriz)))-0.001)
        then jogador {velocidade = (-1, 0), direcao = Oeste }
        else jogador
    Saltar ->
      if (not (emEscad y x matriz))
        then jogador {velocidade = (0, -1)}
        else jogador
    Parar -> jogador {velocidade = (0, 0)}
  where
    tamanhoX = fst $ tamanho jogador
    tamanhoY = snd $ tamanho jogador
    ressaltando = ressalta jogador


          
          


atualizaInimigo :: Personagem -> Mapa -> Maybe Acao -> Personagem
atualizaInimigo inimigo (Mapa ((x,y),_) _ matriz) Nothing = inimigo -- Inércia do movimento
atualizaInimigo inimigo (Mapa ((x,y),_) _ matriz) (Just acao) =
  case acao of
    Subir -> if (emEscad y x matriz) then inimigo { velocidade = (0, -1), direcao = Norte } else inimigo
    Descer -> if (emEscad y x matriz) then inimigo { velocidade = (0, 1), direcao = Sul } else inimigo
    AndarDireita -> 
      if (not (tlvsubirEdescer inimigo)) && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head matriz)))-0.001)
        then inimigo {velocidade = (1, 0), direcao = Este}
        else inimigo
    AndarEsquerda -> 
      if (not (tlvsubirEdescer inimigo)) && ressaltando && (x + tamanhoX/2) < ((fromIntegral (length (head matriz)))-0.001)
        then inimigo {velocidade = (-1, 0), direcao = Oeste }
        else inimigo
    Saltar -> if not (emEscad y x matriz)
        then inimigo {velocidade = (0, -1)}
        else inimigo
    Parar -> inimigo { velocidade = (0, 0) }
  where
    tamanhoX = fst $ tamanho inimigo
    tamanhoY = snd $ tamanho inimigo
    ressaltando = ressalta inimigo


tlvsubirEdescer :: Personagem -> Bool
tlvsubirEdescer  Personagem {direcao = x}= case x of 
            Norte -> True
            Sul -> True
            _ -> False
 
            

emEscad :: Double -> Double -> [[Bloco]] -> Bool
emEscad i j matriz = case matriz !! floor i !! floor j of
                                    Escada -> True
                                    _     -> False