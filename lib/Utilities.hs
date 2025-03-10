{-|
Module      : Utilities
Description : Funções auxiliares
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo com as funções auxiliares utilizadas no desenvolvimento das Tarefas.
-}

module Utilities where

import LI12324
import Graphics.Gloss 
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float 
import Data.List
import Niveis

data State = State {
  images :: Images,
  time :: Tempo,
  levelsList :: Levels,
  currentLevel :: Int,
  currentTheme :: Theme,
  currentMode :: Mode,
  currentMenu :: Menu,
  currentPoints :: Int,
  highScore :: Int,
  selectedButton :: Int,
  exitGame :: Bool
}

data Menu = InGame | Home | Options | Mode | Themes | Levels | Pause | GameOver deriving(Show,Eq)

data Theme = Mario | MarioCat | MarioBear | MarioFrog | MarioAstronaut deriving(Show,Eq)

data Mode = Easy | Medium | Hard deriving(Show,Eq)

type Levels = [Jogo]

type Images = [(Theme,[(String, Picture)])]

-- | Estado inicial do jogo quando se clica no play sem quaisquer alterações

initialState :: State
initialState = State {
  images = [],
  time = 0,
  levelsList = [jogo1,jogo2,jogo3,jogo4,jogo5,jogo6],
  currentLevel = 1,
  currentTheme = Mario,
  currentMode = Easy,
  currentMenu = Home,
  currentPoints = 0,
  highScore = 0,
  selectedButton = 0,
  exitGame = False
}

-- | Escala de cada um dos elementos da matriz do jogo

scaleGame :: Float
scaleGame = 50

-- | Função que troca um elemento de uma lista num determinado índice
updateLevel :: [a] -> (Int,a) -> [a]
updateLevel lvs (i, j) = before ++ [j] ++ after 
  where (before,_:after) = splitAt i lvs

-- | Função para calcular a hitbox de um personagem considerando que px e py estão no respetivo centro

calculaHitbox :: Personagem -> Hitbox
calculaHitbox personagem =
  ((px - tamanhoX / 2, py + tamanhoY / 2), (px + tamanhoX / 2, py - tamanhoY / 2))
  where
    (px, py) = posicao personagem
    (tamanhoX, tamanhoY) = tamanho personagem

-- | Função que calcula a hitbox que verifica colisões em baixo com obstáculos
calculaHitboxEmbaixo :: Personagem -> Hitbox
calculaHitboxEmbaixo jogador = ((px-tx/2.5,py),(px+tx/2.5,py+ty/2))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitbox que verifica colisões em cima com obstáculos
calculaHitboxEmCima :: Personagem -> Hitbox
calculaHitboxEmCima jogador = ((px-tx/2.5,py-ty/2),(px+tx/2.5,py))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitboxe que verifica colisões à esquerda com obstáculos
calculaHitboxEsquerda :: Personagem -> Hitbox
calculaHitboxEsquerda jogador = ((px-tx/1.9,py-ty/2.5),(px,py+ty/2.5))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitbox que verifica colisões à direita com obstáculos
calculaHitboxDireita :: Personagem -> Hitbox
calculaHitboxDireita jogador = ((px,py-ty/2.5),(px+tx/1.9,py+ty/2.5))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitbox que verifica colisões do canto inferior esquerdo com obstáculos
calculaHitboxInfEsquerda :: Personagem -> Hitbox
calculaHitboxInfEsquerda jogador = ((px-tx/2,py),(px,py+ty/2))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitbox que verifica colisões do canto inferior direito com obstáculos
calculaHitboxInfDireita :: Personagem -> Hitbox
calculaHitboxInfDireita jogador = ((px,py),(px+tx/2,py+ty/2))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador

-- | Função que calcula a hitbox que verifica se o jogador está dentro de um bloco 
calculaHitboxDentro :: Personagem -> Hitbox
calculaHitboxDentro jogador = ((px-tx/2.5,py-ty/2.5),(px+tx/2.5,py+ty/2.5))
  where 
    (px,py) = posicao jogador
    (tx,ty) = tamanho jogador


-- | Função que calcula a hitbox de dano do jogador armado
calculaHitboxDano :: Personagem -> Hitbox
calculaHitboxDano jogador = 
    if dir == Este || (dir == Norte && not(emEscada jogador)) then (((px+tx/2),(py+ty/2.2)),((px+tx*1.5),(py-ty/2.2)))
    else if dir == Oeste then (((px-tx*1.5),(py+ty/2.1)),((px-tx/2),(py-ty/2.1)))
    else ((px,py),(px,py))
  where 
    (px,py) = posicao jogador
    dir = direcao jogador
    (tx,ty) = tamanho jogador  


-- | Função para verificar a colisão entre duas hitboxes
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox h1 h2 = colisaoHitboxAux h1 h2 || colisaoHitboxAux h2 h1

colisaoHitboxAux :: Hitbox -> Hitbox -> Bool
colisaoHitboxAux ((x1,y1), (x2,y2)) ((x3,y3),(x4,y4)) = pointInBox (double2Float x3,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x3,double2Float y4) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)
                                                    || pointInBox (double2Float x4,double2Float y3) (double2Float x1,double2Float y1) (double2Float x2,double2Float y2)

-- | Função que retorna uma lista com as hitboxes dos blocos ao receber as posições dos mesmos
hitboxesBlocos :: [Posicao] -> [Hitbox]
hitboxesBlocos = map (\(x,y) -> ((x,y),(x+1,y+1))) 

-- | Função que retorna uma lista com as hitboxes dos colecionaveis
hitboxesColecionaveis :: [(Colecionavel,Posicao)] -> [Hitbox]
hitboxesColecionaveis = map (\(col,(x,y)) -> ((x-0.5,y+0.5),(x+0.5,y-0.5))) 

-- | Verifica se o personagem colide com plataformas ou alçapões
colideComBloco :: Personagem -> Mapa -> Bool
colideComBloco personagem (Mapa _ _ blocos) = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))

-- | Verifica se o personagem colide com alçapões
colideComAlcapoes :: Personagem -> Mapa -> Bool
colideComAlcapoes personagem (Mapa _ _ blocos) = any (\h1-> colisaoHitbox h1 (calculaHitbox personagem)) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))

-- | Troca um bloco pelo vazio
replace :: [[Bloco]] -> Posicao -> [[Bloco]]
replace blocos (px, py) = takeWhile (/= linhapy) blocos ++ [replaceVazio (head $ dropWhile (/= linhapy) blocos) (floor px)] ++ drop 1 (dropWhile (/= linhapy) blocos)
  where
    linhapy = blocos !! ceiling py

replaceVazio :: [Bloco] -> Int -> [Bloco]
replaceVazio [] _ = []
replaceVazio (x:xs) 0 = Vazio : xs 
replaceVazio (x:xs) n = x : replaceVazio xs (n - 1)

-- | Funções que retornam a lista de posições de um determinado bloco no mapa
mapaBlocos :: Mapa -> Bloco -> [Posicao]
mapaBlocos (Mapa _ _ blocos) bloco = [pos | pos <- centerOfBlocos blocos, isBloco bloco (getBloco pos blocos)]

mapaEscadas :: Mapa -> [Posicao]
mapaEscadas (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isEscada (getBloco pos blocos)]

mapaAlcapoes :: Mapa -> [Posicao]
mapaAlcapoes (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos)]

mapaPlataformas :: Mapa -> [Posicao]
mapaPlataformas (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isPlataforma (getBloco pos blocos)]

mapaVazio :: Mapa -> [Posicao]
mapaVazio (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isVazio (getBloco pos blocos)]

mapaTrampolins :: Mapa -> [Posicao]
mapaTrampolins (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isTrampolim (getBloco pos blocos)]

mapaLancas :: Mapa -> [Posicao]
mapaLancas (Mapa _ _ blocos) = [pos | pos <- indicesBlocos blocos, isLanca (getBloco pos blocos)]

mapaPlataformasAlcapoes :: [[Bloco]] -> [Posicao]
mapaPlataformasAlcapoes blocos = [pos | pos <- indicesBlocos blocos, isAlcapao (getBloco pos blocos) || isPlataforma (getBloco pos blocos)]

-- | Função que recebe uma posição e um mapa e devolve a posição da plataforma mais próxima

plataformaProxima :: Posicao -> Mapa -> Posicao 
plataformaProxima (px,py) mapa = foldl (\(pxn,pyn) (xp,yp)-> if (double2Float(abs(pxn-px)))<(double2Float(abs(xp-px))) && notElem (pxn,pyn-1) (mapaPlataformas mapa) then (pxn,pyn) else (xp,yp)) (0,fromIntegral(ceiling py)) linha 
  where linha = (filter (\(x,y)-> y == fromIntegral(ceiling py))(mapaPlataformas mapa))

-- | Função que agrupa as escadas adjacentes
agrupaEscadas :: Mapa -> [[Posicao]]
agrupaEscadas mapa =  agrupaEscadasAux (sortOn fst (mapaEscadas mapa))

agrupaEscadasAux :: [Posicao] -> [[Posicao]]
agrupaEscadasAux [] = []
agrupaEscadasAux [x] = [[x]]
agrupaEscadasAux ((x,y):t)
    | elem (x,y+1) (head r) = ((x,y) : (head r)) : tail r
    | otherwise = [(x,y)] : r
    where r = agrupaEscadasAux t

-- | Função que devolve o primeiro e último elemento de um conjunto de escadas

primUltEscadas :: Mapa -> [[Posicao]]
primUltEscadas mapa = map (\pos->[head pos,last pos]) (agrupaEscadas mapa)

-- | Função que agrupa os alçapões adjacentes
agrupaAlcapoes :: Mapa -> [[Posicao]]
agrupaAlcapoes mapa =  agrupaAlcapoesAux (sortOn snd (mapaAlcapoes mapa))

agrupaAlcapoesAux :: [Posicao] -> [[Posicao]]
agrupaAlcapoesAux [] = []
agrupaAlcapoesAux [x] = [[x]]
agrupaAlcapoesAux ((x,y):t)
    | elem (x+1,y) (head r) = ((x,y) : (head r)) : tail r
    | otherwise = [(x,y)] : r
    where r = agrupaAlcapoesAux t

-- | Função que devolve o primeiro e último elemento de um conjunto de alçapões

primUltAlcapoes :: Mapa -> [[Posicao]]
primUltAlcapoes mapa = map (\pos->[head pos,last pos]) (agrupaAlcapoes mapa)

-- | Função que calcula o tamanho dos alçapões

tamanhoAlcapoes :: Mapa -> [Int]
tamanhoAlcapoes mapa = map (\[(x1,y1),(x2,y2)]-> double2Int(x2-x1+1)) (primUltAlcapoes mapa)

-- | Função que transforma as posições de cada um dos índices dos blocos na sua posição central 

centerOfBlocos :: [[Bloco]] -> [Posicao]
centerOfBlocos blocos = map (\(x,y) -> (x+0.5,y+0.5)) (indicesBlocos blocos)

-- | Função que retorna todas as posições de todos os blocos do mapa
indicesBlocos :: [[Bloco]] -> [Posicao]
indicesBlocos blocos = [(fromIntegral j, fromIntegral i) | i<- [0..height-1], j <- [0..width-1]]
  where
    height = length blocos
    width = length (head blocos)

-- | Função que recebe uma posição e devolve o bloco específico que está nessa posição no mapa

getBloco :: Posicao -> [[Bloco]] -> Bloco
getBloco (i, j) blocos = (blocos !! floor j) !! floor i

-- | Grupo de funções que verifica se recebe o bloco respetivo

isAlcapao :: Bloco -> Bool
isAlcapao Alcapao = True
isAlcapao _ = False

isPlataforma :: Bloco -> Bool
isPlataforma Plataforma = True
isPlataforma _ = False

isEscada :: Bloco -> Bool
isEscada Escada = True
isEscada _ = False

isTrampolim :: Bloco -> Bool
isTrampolim Trampolim = True
isTrampolim _ = False

isVazio :: Bloco -> Bool
isVazio Vazio = True
isVazio _ = False

isLanca :: Bloco -> Bool
isLanca Lanca = True
isLanca _ = False

isBloco :: Bloco -> Bloco -> Bool
isBloco bloco1 bloco2 = bloco1 == bloco2


