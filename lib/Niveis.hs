{-|
Module      : Mapas
Description : Matrizes dos mapas
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo com as matrizes dos mapas e as posições inicial e final do jogador para cada nível.
-}

module Niveis where

import LI12324


jog :: Personagem
jog = Personagem { velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = False, 
                    impulsao = False,
                    posicao = (0,0), 
                    tamanho = (0.7,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este}


-- | Nível 1

mapa1 = Mapa ((1.5, 22.5), Este) (7.5, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','P','P','P','P','P','V','V','V','V','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','P','P','V','V','P','P','P','P','P','V','V','P','P','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','P','P','P','P','P','A','A','A','P','P','P','P','P','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','V','V','V','V','E','V','V','V','E','V','V','V','V','V']
    ,['V','V','P','P','P','P','A','A','A','P','P','P','P','V','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','V','E','V','V','V','V','V','V','V','V','V','E','V','V']
    ,['V','P','P','A','A','A','P','P','P','A','A','A','P','P','V']
    ,['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ])


inim1 :: [Personagem]
inim1 = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (2.5,14.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (10.5,22.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 3, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (7.5,2.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]


colec1 :: [(Colecionavel, Posicao)]
colec1 = [(Moeda,(3.5,10.5)),(Moeda,(4.5,10.5)),(Moeda,(5.5,10.5)),(Moeda,(6.5,10.5)),(Moeda,(7.5,10.5)),(Moeda,(8.5,10.5)),(Moeda,(9.5,10.5)),(Moeda,(10.5,10.5)),(Moeda,(11.5,10.5)),(Moeda,(3.5,18.5)),(Moeda,(4.5,18.5)),(Moeda,(5.5,18.5)),(Moeda,(6.5,18.5)),(Moeda,(7.5,18.5)),(Moeda,(8.5,18.5)),(Moeda,(9.5,18.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(1.5,6.5)),(Moeda,(13.5,6.5)),(Martelo,(7.5,14.5))]

jogo1 ::Jogo
jogo1 = Jogo mapa1 inim1 colec1 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa1


-- | Nível 2

mapa2 = Mapa ((1.5, 22.5), Este) (13.5, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','P','P','P','P','P','A','P']
    ,['V','V','V','V','V','V','T','V','V','V','V','V','V','V','P']
    ,['V','V','V','V','V','P','P','V','V','V','V','V','V','V','P']
    ,['V','V','V','P','P','P','P','V','V','V','V','V','V','T','P']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','E','V','V']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','E','V','V']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','P','P','P']
    ,['P','P','A','A','P','P','V','V','V','V','V','V','V','V','V']
    ,['P','P','V','V','P','E','P','P','V','V','V','V','V','V','V']
    ,['P','P','V','V','P','E','V','V','P','P','V','V','V','V','V']
    ,['P','P','T','T','P','E','V','V','P','P','P','P','V','V','V']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','P','P','P','P','P','P']
    ,['V','V','V','V','V','V','V','P','P','V','V','V','V','V','P']
    ,['V','V','V','V','V','P','P','P','E','V','V','V','V','V','P']
    ,['V','V','V','P','P','P','P','P','E','V','V','V','V','V','P']
    ,['P','P','P','P','P','P','P','P','P','P','T','T','T','T','P']
    ])

inim2 :: [Personagem]
inim2 = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (14.5,2.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (0.5,10.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (14.5,18.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    posicao = (12.5,6.5), 
                    tamanho = (0.8,0.8), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]


colec2 :: [(Colecionavel, Posicao)]
colec2 = [(Moeda,(10.5,22.5)),(Moeda,(11.5,22.5)),(Moeda,(12.5,22.5)),(Moeda,(13.5,22.5)),(Moeda,(10.5,21.5)),(Moeda,(11.5,21.5)),(Moeda,(12.5,21.5)),(Moeda,(13.5,21.5)),(Moeda,(10.5,20.5)),(Moeda,(11.5,20.5)),(Moeda,(12.5,20.5)),(Moeda,(13.5,20.5)),(Moeda,(9.5,18.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(12.5,18.5)),(Moeda,(6.5,14.5)),(Moeda,(6.5,13.5)),(Moeda,(7.5,13.5)),(Moeda,(2.5,12.5)),(Moeda,(3.5,12.5)),(Moeda,(2.5,13.5)),(Moeda,(3.5,13.5)),(Moeda,(7.5,6.5)),(Moeda,(8.5,6.5)),(Moeda,(9.5,6.5)),(Moeda,(10.5,6.5)),(Moeda,(11.5,6.5)),(Moeda,(12.5,6.5)),(Moeda,(8.5,2.5)),(Moeda,(9.5,2.5)),(Moeda,(10.5,2.5)),(Moeda,(11.5,2.5)),(Moeda,(12.5,2.5)),(Moeda,(13.5,8.5)),(Moeda,(14.5,8.5)),(Moeda,(13.5,9.5)),(Moeda,(14.5,9.5)),(Martelo,(7.5,14.5))]

jogo2 ::Jogo
jogo2 = Jogo mapa2 inim2 colec2 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa2



mapa3 = Mapa ((1.5, 22.5), Este) (7.5, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ])

abreviaBlocos :: [[Char]] -> [[Bloco]]
abreviaBlocos = map (\bloco -> replaceValue bloco) 

replaceValue :: [Char] -> [Bloco]
replaceValue [] = []
replaceValue (x:xs)
    | x == 'V' = Vazio : replaceValue xs
    | x == 'P' = Plataforma : replaceValue xs
    | x == 'E' = Escada : replaceValue xs
    | x == 'A' = Alcapao : replaceValue xs
    | x == 'T' = Trampolim : replaceValue xs
    | otherwise = replaceValue xs
