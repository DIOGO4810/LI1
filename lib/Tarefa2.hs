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
import Utilities
import Niveis

valida :: Jogo -> Bool
valida jogo =
  temChao && inimigosRessaltam && posicoesIniciaisValidas && numeroMinimoInimigos && vidasFantasmas && semBlocosEmPersCole && restricoesEscadas (mapa jogo) && restricoesAlcapoes (jogador jogo) (mapa jogo)
  where
    (Mapa ((xi,yi),dir) (xf,yf) blocos) = mapa jogo
    listaInimigos = inimigos jogo
    jogadorJogo = jogador jogo
    listaColecionaveis = colecionaveis jogo
    (tamanhoX, tamanhoY) = tamanho (jogador jogo)

    -- | 1. O mapa tem "chão", ou seja, uma plataforma que impede que o jogador ou outro personagem caia fora do mapa.
    temChao = all (\b -> isPlataforma b || isTrampolim b || isLanca b) (last blocos)

    -- | 2. Todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.
    inimigosRessaltam = all (\inimigo -> ressalta inimigo) listaInimigos && not (ressalta jogadorJogo)

    -- | 3. A posição inicial de um jogador não pode colidir com a posição inicial de outro personagem.
    posicoesIniciaisValidas = all (\inimigo -> posicao inimigo /= (xi,yi)) listaInimigos

    -- | 4. Número mínimo de inimigos: 2 (dois).
    numeroMinimoInimigos = length listaInimigos >= 2

    -- | 5. Inimigos Fantasma têm exatamente 1 (uma) vida.
    vidasFantasmas = all (\inimigo -> if tipo inimigo == Fantasma then vida inimigo == 1 else True) listaInimigos

    -- | 6. Escadas não podem começar/terminar em alçapões, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma.
    restricoesEscadas mapa = all (\[(x1,y1),(x2,y2)] -> (if (elem (x1,y1-1) posplataformas) then (notElem (x2,y2+1) posalcapoes) else if (elem (x1,y1-1) posvazio) then (elem (x2,y2+1) posplataformas) else False)) (escadasadj)
          where escadasadj = primUltEscadas mapa
                posplataformas = mapaPlataformas mapa
                posalcapoes = mapaAlcapoes mapa
                posvazio = mapaVazio mapa

    -- | 7. Alçapões não podem ser menos largos que o jogador.
    restricoesAlcapoes jogador mapa= all (\tamalcapoes -> fromIntegral tamalcapoes>tamanhoX) (tamanhoAlcapoes mapa)
      where tamanhoX = fst $ tamanho jogador

    -- | 8. Não podem existir personagens nem colecionáveis "dentro" de plataformas ou alçapões.
    semBlocosEmPersCole = emvazio (xi,yi) blocos && all (\posinim -> emvazio posinim blocos) posinimigos && all (\(x2, y2) -> emvazio (x2,y2) blocos) listaposcolecionaveis
        where listaposcolecionaveis = map snd listaColecionaveis
              emvazio (i,j) blocos = getBloco (i,j) blocos == Vazio
              posinimigos = map posicao listaInimigos

