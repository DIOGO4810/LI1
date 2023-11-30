{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Utilities
import Data.List
import GHC.Float (double2Int)

valida :: Jogo -> Bool
valida jogo =
  temChao (mapa jogo) && inimigosRessaltam && posicoesIniciaisValidas && numeroMinimoInimigos && vidasFantasmas && semBlocosEmPersCole (mapa jogo) (colecionaveis jogo)
  where
    mapaJogo = mapa jogo
    listainimigos = inimigos jogo
    jogadorJogo = jogador jogo
    listadecolecionaveis = colecionaveis jogo
    (tamanhoX, tamanhoY) = tamanho (jogador jogo)

    -- | 1. O mapa tem "chão", ou seja, uma plataforma que impede que o jogador ou outro personagem caia fora do mapa.
    temChao :: Mapa -> Bool
    temChao (Mapa _ _ blocos) = any (elem Plataforma) blocos

    -- | 2. Todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.
    inimigosRessaltam = all (\inimigo -> ressalta inimigo) listainimigos && not (ressalta jogadorJogo)

    -- | 3. A posição inicial de um jogador não pode colidir com a posição inicial de outro personagem.
    posicoesIniciaisValidas = all (\inimigo -> posicao inimigo /= posicao jogadorJogo) listainimigos

    -- | 4. Número mínimo de inimigos: 2 (dois).
    numeroMinimoInimigos = length listainimigos >= 2

    -- | 5. Inimigos Fantasma têm exatamente 1 (uma) vida.
    vidasFantasmas = all (\inimigo -> if tipo inimigo == Fantasma then vida inimigo == 1 else True) listainimigos

    -- | 6. Escadas não podem começar/terminar em alçapões, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma.
    restricoesEscadas mapa = all (\[(x1,y1),(x2,y2)] -> (if (elem (x1,y1-1) posplataformas) then (notElem (x2,y2+1) posalcapoes) else if (elem (x1,y1-1) posvazio) then (elem (x2,y2+1) posplataformas) else False)) (escadasadj)
          where escadasadj = primUltEscadas mapa
                posplataformas = mapaPlataformas mapa
                posalcapoes = mapaAlcapoes mapa
                posvazio = mapaVazio mapa

    -- | 7. Alçapões não podem ser menos largos que o jogador.
    restricoesAlcapoes tamanhoX mapa= all (\tamalcapoes -> tamalcapoes>tamanhoX) (tamanhoAlcapoes mapa)

    -- | 8. Não podem existir personagens nem colecionáveis "dentro" de plataformas ou alçapões.
      
    semBlocosEmPersCole :: Mapa -> [(Colecionavel, Posicao)] -> Bool
    semBlocosEmPersCole (Mapa ((x1, y1), _) _ blocos) listadecolecionaveis = emvazio y1 x1 blocos && all (\(x2, y2) -> emvazio y2 x2 blocos) listaposcolecionaveis
        where
            listaposcolecionaveis = map snd listadecolecionaveis
            emvazio i j blocos = case blocos !! floor i !! floor j of
                                    Vazio -> True
                                    _     -> False

