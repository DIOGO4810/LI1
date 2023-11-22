{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida jogo =
  temChao (mapa jogo) && inimigosRessaltam && posicoesIniciaisValidas && numeroMinimoInimigos && vidasFantasmas && restricoesEscadas && restricoesAlcapoes && semBlocosEmPersCole (mapa jogo) (colecionaveis jogo)
  where
    mapaJogo = mapa jogo
    listainimigos = inimigos jogo
    jogadorJogo = jogador jogo
    listadecolecionaveis = colecionaveis jogo

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
    restricoesEscadas = all (\(pos, direcao) -> escadaValida pos direcao mapaJogo) (mapaEscadas mapaJogo)

    -- | 7. Alçapões não podem ser menos largos que o jogador.
    restricoesAlcapoes = all (\(pos, direcao) -> alcapaoValido pos direcao jogadorJogo mapaJogo) (mapaAlcapoes mapaJogo)

    -- | 8. Não podem existir personagens nem colecionáveis "dentro" de plataformas ou alçapões.
      
    semBlocosEmPersCole :: Mapa -> [(Colecionavel, Posicao)] -> Bool
    semBlocosEmPersCole (Mapa ((x1, y1), _) _ matriz) listadecolecionaveis = emvazio y1 x1 matriz && all (\(x2, y2) -> emvazio y2 x2 matriz) listaposcolecionaveis
        where
            listaposcolecionaveis = map snd listadecolecionaveis
            emvazio i j matriz = case matriz !! floor i !! floor j of
                                    Vazio -> True
                                    _     -> False

-- | Funções auxiliares

mapaBlocos :: Mapa -> [[Bloco]]
mapaBlocos (Mapa _ _ matriz) = matriz

mapaEscadas :: Mapa -> [(Posicao, Direcao)]
mapaEscadas (Mapa _ _ blocos) = [(pos, direcao) | (pos, direcao) <- indicesBlocos blocos, isEscada (getBloco pos blocos)]

mapaAlcapoes :: Mapa -> [(Posicao, Direcao)]
mapaAlcapoes (Mapa _ _ blocos) = [(pos, direcao) | (pos, direcao) <- indicesBlocos blocos, isAlcapao (getBloco pos blocos)]

indicesBlocos :: [[a]] -> [(Posicao, Direcao)]
indicesBlocos blocos = [( (fromIntegral i, fromIntegral j), Este) | i <- [0..height-1], j <- [0..width-1]]
  where
    height = length blocos
    width = length (head blocos)

getBloco :: Posicao -> [[a]] -> a
getBloco (i, j) blocos = (blocos !! floor i) !! floor j

isEscada :: Bloco -> Bool
isEscada Escada = True
isEscada _ = False

isAlcapao :: Bloco -> Bool
isAlcapao Alcapao = True
isAlcapao _ = False

escadaValida :: Posicao -> Direcao -> Mapa -> Bool
escadaValida posicao@(i, j) direcao mapa =
  case getBloco posicao (mapaBlocos mapa) of
    Escada ->
      case direcao of
        Norte -> i > 0 && getBloco (i - 1, j) (mapaBlocos mapa) /= Alcapao
        Sul   -> i < fromIntegral (length (mapaBlocos mapa)) - 1 && getBloco (i + 1, j) (mapaBlocos mapa) /= Alcapao
        Este  -> j < fromIntegral (length (head (mapaBlocos mapa))) - 1 && getBloco (i, j + 1) (mapaBlocos mapa) /= Alcapao
        Oeste -> j > 0 && getBloco (i, j - 1) (mapaBlocos mapa) /= Alcapao
    _ -> True


alcapaoValido :: Posicao -> Direcao -> Personagem -> Mapa -> Bool
alcapaoValido posicao@(i, j) direcao jogador mapa =
  case getBloco posicao (mapaBlocos mapa) of
    Alcapao ->
      case direcao of
        Norte -> i > 0 && getBloco (i - 1, j) (mapaBlocos mapa) == Plataforma
        Sul   -> i < fromIntegral (length (mapaBlocos mapa)) - 1 && getBloco (i + 1, j) (mapaBlocos mapa) == Plataforma
        Este  -> j < fromIntegral (length (head (mapaBlocos mapa))) - 1 && getBloco (i, j + 1) (mapaBlocos mapa) == Plataforma
        Oeste -> j > 0 && getBloco (i, j - 1) (mapaBlocos mapa) == Plataforma
    _ -> True


