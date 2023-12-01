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

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesInimigos acaoJogador jogo =
  Jogo
    { jogador = atualizaJogador (jogador jogo) (mapa jogo) acaoJogador
    , inimigos = zipWith (\inimigo acao -> atualizaInimigo inimigo (mapa jogo) acao) (inimigos jogo) acoesInimigos
    , mapa = mapa jogo
    , colecionaveis = colecionaveis jogo
    }

atualizaJogador ::  Personagem ->Mapa -> Maybe Acao -> Personagem
atualizaJogador jogador (Mapa ((x,y),_) _ matriz) Nothing = jogador -- Inércia do movimento
atualizaJogador jogador (Mapa ((x,y),d) _ matriz) (Just acao) =
  case acao of
    Subir -> if (emEscad y x matriz) then jogador { velocidade = (0, -1), direcao = Norte } else jogador
    Descer -> if (emEscad y x matriz) then jogador { velocidade = (0, 1), direcao = Sul } else jogador
    AndarDireita ->if (not (tlvsubirEdescer jogador)) && ressalta && (x+tamanhoX) < fromIntegral(length (head matriz)) then jogador { velocidade = (0, 1), direcao = Este} else jogador
    AndarEsquerda -> jogador { velocidade = (0, -1)}
    Saltar -> if not (emEscad y x matriz) then jogador { velocidade = (0, -1) } else jogador
    Parar -> jogador { velocidade = (0, 0) }
    where (tamanhoX, tamanhoY) = Personagem {tamanho}
          ressaltando = ressalta Personagem
          
          


atualizaInimigo :: Personagem -> Mapa -> Maybe Acao -> Personagem
atualizaInimigo inimigo (Mapa ((x,y),_) _ matriz) Nothing = inimigo -- Inércia do movimento
atualizaInimigo inimigo (Mapa ((x,y),_) _ matriz) (Just acao) =
  case acao of
    Subir -> if (emEscad y x matriz) then inimigo { velocidade = (0, -1), direcao = Norte } else inimigo
    Descer -> if (emEscad y x matriz) then inimigo { velocidade = (0, 1), direcao = Sul } else inimigo
    AndarDireita -> inimigo { velocidade = (0, 1)}
    AndarEsquerda -> inimigo { velocidade = (0, -1)}
    Saltar -> inimigo { velocidade = (0, -1) }
    Parar -> inimigo { velocidade = (0, 0) }





tlvsubirEdescer :: Personagem -> Bool
tlvsubirEdescer  Personagem {direcao = x}= case x of 
            Norte -> True
            Sul -> True
            _ -> False
 
            

emEscad :: Double -> Double -> [[Bloco]] -> Bool
emEscad i j matriz = case matriz !! floor i !! floor j of
                                    Escada -> True
                                    _     -> False