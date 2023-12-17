{-|
Module      : Mapas
Description : Matrizes dos mapas
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo com as matrizes dos mapas e as posições inicial e final do jogador para cada nível.
-}

module Mapas where

import LI12324


-- | Matriz do mapa do nível 1

mapa1 = Mapa ((1.5, 23.5), Este) (7.5, 2.5)
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


inm :: [Personagem]
inm = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (1.5,2.5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]

jog :: Personagem
jog = Personagem {  velocidade = (0,0),
                    tipo = Jogador,
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = False, 
                    posicao = (1.5,22.5), 
                    tamanho = (0.7,1), 
                    aplicaDano = (True, 10), 
                    direcao = Este}


colec :: [(Colecionavel, Posicao)]
colec = [(Moeda,(3.5,10.0)),(Moeda,(4.5,10.0)),(Moeda,(5.5,10.0)),(Moeda,(6.5,10.0)),(Moeda,(7.5,10.0)),(Moeda,(8.5,10.0)),(Moeda,(9.5,10.0)),(Moeda,(10.5,10.0)),(Moeda,(11.5,10.0)),(Moeda,(3.5,18.0)),(Moeda,(4.5,18.0)),(Moeda,(5.5,18.0)),(Moeda,(6.5,18.0)),(Moeda,(7.5,18.0)),(Moeda,(8.5,18.0)),(Moeda,(9.5,18.0)),(Moeda,(10.5,18.0)),(Moeda,(11.5,18.0)),(Martelo,(7.5,14.0))]

jogoSamp ::Jogo
jogoSamp = Jogo mapa1 inm colec jog

