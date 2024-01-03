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
                    posicao = (0,0), 
                    tamanho = (0.5,1), 
                    aplicaDano = (False, 0), 
                    direcao = Este}


-- | Nível 1

mapa1 = Mapa ((1.5, 22.5), Este) (7.5, 2.0)
    [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Plataforma,Plataforma,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio]
    ,[Vazio,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Plataforma,Alcapao,Alcapao,Alcapao,Plataforma,Plataforma,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
    ,[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
    ]


inim1 :: [Personagem]
inim1 = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
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
