{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Data.List

valida :: Jogo -> Bool
valida jogo =
  temChao (mapa jogo) && inimigosRessaltam && posicoesIniciaisValidas && numeroMinimoInimigos && vidasFantasmas && semBlocosEmPersCole (mapa jogo) (colecionaveis jogo)
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
    restricoesEscadas mapa = all (\[(x1,y1),(x2,y2)] -> (if (elem (x1,y1-1) posplataformas) then (notElem (x2,y2+1) posalcapoes) else if (elem (x1,y1-1) posvazio) then (elem (x2,y2+1) posplataformas) else False)) (escadasadj)
          where escadasadj = primUltEscadas mapa
                posplataformas = mapaPlataformas mapa
                posalcapoes = mapaAlcapoes mapa
                posvazio = mapaVazio mapa

    -- | 7. Alçapões não podem ser menos largos que o jogador.
    -- restricoesAlcapoes = all (\(pos, direcao) -> alcapaoValido pos direcao jogadorJogo mapaJogo) (mapaAlcapoes mapaJogo)

    -- | 8. Não podem existir personagens nem colecionáveis "dentro" de plataformas ou alçapões.
      
    semBlocosEmPersCole :: Mapa -> [(Colecionavel, Posicao)] -> Bool
    semBlocosEmPersCole (Mapa ((x1, y1), _) _ matriz) listadecolecionaveis = emvazio y1 x1 matriz && all (\(x2, y2) -> emvazio y2 x2 matriz) listaposcolecionaveis
        where
            listaposcolecionaveis = map snd listadecolecionaveis
            emvazio i j matriz = case matriz !! floor i !! floor j of
                                    Vazio -> True
                                    _     -> False

-- | Funções auxiliares

mapaEscadas :: Mapa -> [Posicao]
mapaEscadas (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isEscada (getBloco pos blocos)]

mapaAlcapoes :: Mapa -> [Posicao]
mapaAlcapoes (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos)]

mapaPlataformas :: Mapa -> [Posicao]
mapaPlataformas (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isPlataforma (getBloco pos blocos)]

mapaVazio :: Mapa -> [Posicao]
mapaVazio (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isVazio (getBloco pos blocos)]

indicesBlocos :: [[Bloco]] -> [Posicao]
indicesBlocos blocos = [(fromIntegral j, fromIntegral i) | i<- [0..height-1], j <- [0..width-1]]
  where
    height = length blocos
    width = length (head blocos)

getBloco :: Posicao -> [[Bloco]] -> Bloco
getBloco (i, j) blocos = (blocos !! floor j) !! floor i

isEscada :: Bloco -> Bool
isEscada Escada = True
isEscada _ = False

isAlcapao :: Bloco -> Bool
isAlcapao Alcapao = True
isAlcapao _ = False

isPlataforma :: Bloco -> Bool
isPlataforma Plataforma = True
isPlataforma _ = False

isVazio :: Bloco -> Bool
isVazio Vazio = True
isVazio _ = False

agrupaEscadas :: Mapa -> [[Posicao]]
agrupaEscadas mapa =  agrupaEscadasAux (sortOn fst (mapaEscadas mapa))

agrupaEscadasAux :: [Posicao] -> [[Posicao]]
agrupaEscadasAux [] = []
agrupaEscadasAux [x] = [[x]]
agrupaEscadasAux ((x,y):t)
    | elem (x,y+1) (head r) = ((x,y) : (head r)) : tail r
    | otherwise = [(x,y)] : r
    where r = agrupaEscadasAux t

primUltEscadas :: Mapa -> [[Posicao]]
primUltEscadas mapa = map (\pos->[head pos,last pos]) (agrupaEscadas mapa)

