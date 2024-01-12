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
                    escudo = (False,0),
                    kickback = (False,0),
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
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (7.5,6.0), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (7.5,4.7), 
                    tamanho = (1,0.5), 
                    aplicaDano = (False,0), 
                    direcao = Sul},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (12.5,14.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (12.5,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,10.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,18.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0),
                    direcao = Oeste}]


colec1 :: [(Colecionavel, Posicao)]
colec1 = [(Moeda,(3.5,10.5)),(Moeda,(4.5,10.5)),(Moeda,(5.5,10.5)),(Moeda,(6.5,10.5)),(Moeda,(8.5,10.5)),(Moeda,(9.5,10.5)),(Moeda,(10.5,10.5)),(Moeda,(11.5,10.5)),(Moeda,(3.5,18.5)),(Moeda,(4.5,18.5)),(Moeda,(5.5,18.5)),(Moeda,(6.5,18.5)),(Moeda,(7.5,18.5)),(Moeda,(8.5,18.5)),(Moeda,(9.5,18.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(1.5,6.5)),(Moeda,(2.5,6.5)),(Moeda,(13.5,6.5)),(Moeda,(12.5,6.5)),(Martelo,(7.5,14.5)),(Escudo,(7.5,10.5))]

jogo1 ::Jogo
jogo1 = Jogo mapa1 inim1 colec1 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa1


-- | Nível 2
mapa2 = Mapa ((1.5, 22.5), Este) (13.5, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['P','P','V','V','V','V','V','V','P','P','P','P','P','A','P']
    ,['V','V','V','V','V','V','T','V','V','V','V','V','V','V','P']
    ,['V','V','V','V','V','P','P','V','V','V','V','V','V','V','P']
    ,['V','V','V','P','L','P','P','V','V','V','V','V','V','T','P']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','E','V','V']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','E','V','V']
    ,['V','E','V','V','V','V','V','V','V','V','V','P','P','P','P']
    ,['P','P','A','A','P','P','V','V','V','V','V','V','V','V','V']
    ,['P','P','V','V','P','E','P','P','V','V','V','V','V','V','V']
    ,['P','P','V','V','P','E','V','V','L','P','V','V','V','V','V']
    ,['P','P','T','T','P','E','V','V','P','P','L','P','V','V','V']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','E','V']
    ,['V','V','V','V','V','V','V','V','V','P','P','P','P','P','P']
    ,['V','V','V','V','V','V','V','P','P','V','V','V','V','V','P']
    ,['V','V','V','V','V','P','L','P','E','V','V','V','V','V','P']
    ,['V','V','V','P','L','P','P','P','E','V','V','V','V','V','P']
    ,['P','P','P','P','P','P','P','P','P','P','T','T','T','T','P']
    ])

inim2 :: [Personagem]
inim2 = [Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0), 
                    posicao = (1.0,2.0), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.0,0.7), 
                    tamanho = (1,0.5), 
                    aplicaDano = (False,0), 
                    direcao = Sul},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0), 
                    posicao = (14.5,2.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0), 
                    posicao = (0.5,10.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (14.5,18.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (12.5,6.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}]


colec2 :: [(Colecionavel, Posicao)]
colec2 = [(Moeda,(10.5,22.5)),(Moeda,(11.5,22.5)),(Moeda,(12.5,22.5)),(Moeda,(13.5,22.5)),(Moeda,(10.5,21.5)),(Moeda,(11.5,21.5)),(Moeda,(12.5,21.5)),(Moeda,(13.5,21.5)),(Moeda,(10.5,20.5)),(Moeda,(11.5,20.5)),(Moeda,(12.5,20.5)),(Moeda,(13.5,20.5)),(Moeda,(9.5,18.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(12.5,18.5)),(Moeda,(6.5,14.5)),(Moeda,(6.5,13.5)),(Moeda,(7.5,13.5)),(Moeda,(2.5,12.5)),(Moeda,(3.5,12.5)),(Moeda,(2.5,13.5)),(Moeda,(3.5,13.5)),(Moeda,(7.5,6.5)),(Moeda,(8.5,6.5)),(Moeda,(9.5,6.5)),(Moeda,(10.5,6.5)),(Moeda,(11.5,6.5)),(Moeda,(12.5,6.5)),(Moeda,(8.5,2.5)),(Moeda,(9.5,2.5)),(Moeda,(10.5,2.5)),(Moeda,(11.5,2.5)),(Moeda,(12.5,2.5)),(Moeda,(13.5,8.5)),(Moeda,(14.5,8.5)),(Moeda,(13.5,9.5)),(Moeda,(14.5,9.5)),(Martelo,(7.5,14.5)),(Escudo,(5.5,4.5))]

jogo2 ::Jogo
jogo2 = Jogo mapa2 inim2 colec2 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa2


-- | Nível 3
mapa3 = Mapa ((3.5, 22.5), Oeste) (14.0, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','P','V','V','P','V','V']
    ,['V','V','V','V','V','V','V','V','V','P','V','V','P','V','V']
    ,['V','V','V','V','V','V','V','V','V','P','V','V','P','V','V']
    ,['V','V','P','P','P','V','V','V','V','P','P','P','P','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['T','V','V','V','P','L','V','V','V','V','P','V','V','V','T']
    ,['P','P','V','V','P','P','P','A','A','A','P','V','V','P','P']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','T','P','V','V','V','V','L','P','T','V','V','V']
    ,['V','V','P','P','P','A','A','A','P','P','P','P','P','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['T','V','V','V','P','L','V','V','V','V','P','V','V','V','T']
    ,['P','P','V','V','P','P','P','A','A','A','P','V','V','P','P']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','P','V','V','V','V']
    ,['V','V','V','T','P','V','V','V','V','L','P','T','V','V','V']
    ,['V','V','P','P','P','A','A','A','P','P','P','P','P','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','P','V','V','V','V','V','V','V','V','V','V']
    ,['T','V','V','V','P','L','V','V','V','V','V','V','V','V','T']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ])

inim3 :: [Personagem]
inim3 = [Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (11.0,2.0), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (11.0,0.7), 
                    tamanho = (1,0.5), 
                    aplicaDano = (False,0), 
                    direcao = Sul},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (5.5,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (5.5,6.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (9.5,10.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (5.5,14.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
          Personagem {velocidade = (0,0), 
                      tipo = Fantasma, 
                      emEscada = False, 
                      vida = 1, 
                      pontos = 0, 
                      ressalta = True, 
                      impulsao = False,
                      escudo = (False,0),
                      kickback = (False,0),
                      posicao = (9.5,18.5), 
                      tamanho = (0.8,1.0), 
                      aplicaDano = (False, 0), 
                      direcao = Oeste}]


colec3 :: [(Colecionavel, Posicao)]
colec3 = [(Moeda,(0.5,21.5)),(Moeda,(0.5,20.5)),(Moeda,(0.5,19.5)),(Moeda,(0.5,18.5)),(Moeda,(3.5,17.5)),(Moeda,(3.5,16.5)),(Moeda,(3.5,15.5)),(Moeda,(3.5,14.5)),(Moeda,(0.5,13.5)),(Moeda,(0.5,12.5)),(Moeda,(0.5,11.5)),(Moeda,(0.5,10.5)),(Moeda,(3.5,9.5)),(Moeda,(3.5,8.5)),(Moeda,(3.5,7.5)),(Moeda,(3.5,6.5)),(Moeda,(0.5,5.5)),(Moeda,(0.5,4.5)),(Moeda,(0.5,3.5)),(Moeda,(0.5,2.5)),(Moeda,(5.5,6.5)),(Moeda,(6.5,6.5)),(Moeda,(7.5,6.5)),(Moeda,(8.5,6.5)),(Moeda,(9.5,6.5)),(Moeda,(5.5,10.5)),(Moeda,(6.5,10.5)),(Moeda,(7.5,10.5)),(Moeda,(8.5,10.5)),(Moeda,(9.5,10.5)),(Moeda,(5.5,14.5)),(Moeda,(6.5,14.5)),(Moeda,(8.5,14.5)),(Moeda,(9.5,14.5)),(Moeda,(5.5,18.5)),(Moeda,(6.5,18.5)),(Moeda,(7.5,18.5)),(Moeda,(8.5,18.5)),(Moeda,(9.5,18.5)),(Moeda,(5.5,22.5)),(Moeda,(6.5,22.5)),(Moeda,(7.5,22.5)),(Moeda,(8.5,22.5)),(Moeda,(9.5,22.5)),(Moeda,(10.5,22.5)),(Moeda,(11.5,22.5)),(Moeda,(12.5,22.5)),(Moeda,(13.5,22.5)),(Moeda,(14.5,21.5)),(Moeda,(14.5,20.5)),(Moeda,(14.5,19.5)),(Moeda,(14.5,18.5)),(Moeda,(11.5,17.5)),(Moeda,(11.5,16.5)),(Moeda,(11.5,15.5)),(Moeda,(11.5,14.5)),(Moeda,(14.5,13.5)),(Moeda,(14.5,12.5)),(Moeda,(14.5,11.5)),(Moeda,(14.5,10.5)),(Moeda,(11.5,9.5)),(Moeda,(11.5,8.5)),(Moeda,(11.5,7.5)),(Moeda,(11.5,6.5)),(Martelo,(3.5,4.5))]

jogo3 ::Jogo
jogo3 = Jogo mapa3 inim3 colec3 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa3


-- | Nível 4
mapa4 = Mapa ((1.5, 22.5), Este) (7.5, 2.0) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','P','P','P','P','P','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','E','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','P','V','V','V','P','P','P','P','V','T','V','P','P','V']
    ,['V','V','V','V','V','V','V','V','V','V','P','V','E','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','P','P','A','A','A','P','T','P','L','V','V']
    ,['T','P','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','T','A','P','A','P','L','V','A','A','P','P','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','T']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','T','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','P','P','V','V']
    ,['V','V','V','V','V','V','P','V','P','V','V','V','V','V','V']
    ,['V','V','V','V','V','L','P','V','P','V','V','V','V','V','V']
    ,['P','P','P','P','T','P','P','P','P','P','P','P','P','P','T']
    ])
    

inim4 :: [Personagem]
inim4 = [Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (7.5,6.0), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (7.5,4.7), 
                    tamanho = (1,0.5), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (14,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (12.5,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (7.7,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste}                    
                    ]


colec4 :: [(Colecionavel, Posicao)]
colec4 = [(Moeda,(6.5,14.5)),(Moeda,(4.5,14.5)),(Moeda,(5.5,10.5)),(Moeda,(6.5,10.5)),(Moeda,(8.5,10.5)),(Moeda,(9.5,10.5)),(Moeda,(10.5,10.5)),(Moeda,(11.5,10.5)),(Moeda,(6.5,18.5)),(Moeda,(7.5,18.5)),(Moeda,(8.5,18.5)),(Moeda,(9.5,18.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(13.5,6.5)),(Moeda,(12.5,6.5)),(Martelo,(14,22.5)),(Escudo,(7.5,10.5)),(Escudo,(3.5,22.5))]

jogo4 ::Jogo
jogo4 = Jogo mapa4 inim4 colec4 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa4


-- | Nível 5
mapa5 = Mapa ((2.5, 21.5), Este) (14, 12.5) (abreviaBlocos $
    [['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','P','A','A','P','A','A','P','P','V','V','V']
    ,['V','V','V','V','E','V','V','P','V','V','E','P','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','E','P','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','E','P','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','E','P','A','A','L']
    ,['V','T','V','V','V','V','V','P','T','L','P','P','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','P','V','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','P','V','V','V','V']
    ,['V','V','V','T','V','V','V','P','P','P','P','P','A','A','P']
    ,['V','V','V','V','V','V','V','P','V','V','V','E','V','V','V']
    ,['V','V','V','V','V','V','V','P','V','V','V','E','V','V','V']
    ,['V','V','V','V','V','V','T','P','P','P','P','P','L','P','P']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['P','P','P','T','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','T','V','V','V','V','V','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','P','V']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['P','P','P','V','V','V','P','V','V','V','P','V','V','V','V']
    ,['P','P','P','P','P','P','P','P','P','P','P','T','P','P','P']
    ])
    

inim5 :: [Personagem]
inim5 = [Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (9,9), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (9,7.7), 
                    tamanho = (1,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (5,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,15.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (17,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (9,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (8,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (12.5,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False, 0),
                    posicao = (4,22.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este},                                      
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (14,9.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False, 0), 
                    direcao = Este}
                    ]  


colec5 :: [(Colecionavel, Posicao)]
colec5 = [(Moeda,(6.5,21.5)),(Moeda,(1.5,6.5)),(Moeda,(3.5,9.5)),(Moeda,(6.5,12.5)),(Moeda,(8.5,18.5)),(Moeda,(10.5,21.5)),(Moeda,(8.5,6.5)),(Moeda,(9.5,6.5)),(Moeda,(9.5,5.5)),(Moeda,(8.5,5.5)),(Moeda,(8.5,4.5)),(Moeda,(9.5,4.5)),(Moeda,(8.5,3.5)),(Moeda,(9.5,3.5)),(Escudo,(7.5,1.5)),(Martelo,(13.5,19.5))]

jogo5 ::Jogo
jogo5 = Jogo mapa5 inim5 colec5 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa5


-- | Nível 6
mapa6 = Mapa ((0.5, 22.5), Este) (3.5, 4.0) (abreviaBlocos $
    [['P','P','P','P','V','V','V','P','V','V','V','P','V','V','V']
    ,['V','V','V','P','V','V','V','P','V','V','V','P','V','V','V']
    ,['V','V','V','P','V','L','V','P','V','L','V','P','V','L','V']
    ,['V','V','V','V','V','P','V','P','V','P','V','P','V','P','V']
    ,['V','V','V','V','V','P','T','V','V','P','T','V','V','P','T']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','E']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','E']
    ,['V','V','P','V','V','V','P','V','V','V','P','V','V','V','E']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['V','T','V','V','L','V','V','V','L','V','V','V','V','V','L']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['E','P','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['E','P','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['E','P','A','A','A','P','A','A','A','P','A','A','A','P','P']
    ,['E','V','V','V','V','V','V','V','V','V','V','V','V','V','E']
    ,['E','V','V','V','V','V','V','V','V','V','V','V','V','V','V']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ,['V','V','V','V','V','P','V','V','V','P','V','V','V','V','E']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','E']
    ,['V','V','V','V','V','V','V','V','V','V','V','V','V','V','E']
    ,['V','V','V','P','V','V','V','P','V','V','V','P','V','V','E']
    ,['V','V','T','P','T','L','T','P','T','L','T','P','T','L','E']
    ,['P','P','P','P','P','P','P','P','P','P','P','P','P','P','P']
    ])

inim6 :: [Personagem]
inim6 = [Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,4.0), 
                    tamanho = (1.6,2), 
                    aplicaDano = (True, 3.0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Barril, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,2.7), 
                    tamanho = (1,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
          Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (1.5,16.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    impulsao = False,
                    escudo = (False,0),
                    kickback = (False,0),
                    posicao = (13.5,10.5), 
                    tamanho = (0.8,1.0), 
                    aplicaDano = (False,0), 
                    direcao = Este}]  


colec6 :: [(Colecionavel, Posicao)]
colec6 = [(Moeda,(2.5,21.5)),(Moeda,(2.5,20.5)),(Moeda,(2.5,19.5)),(Moeda,(2.5,18.5)),(Moeda,(3.5,18.5)),(Moeda,(4.5,18.5)),(Moeda,(4.5,19.5)),(Moeda,(4.5,20.5)),(Moeda,(4.5,21.5)),(Moeda,(6.5,21.5)),(Moeda,(6.5,20.5)),(Moeda,(6.5,19.5)),(Moeda,(6.5,18.5)),(Moeda,(7.5,18.5)),(Moeda,(8.5,18.5)),(Moeda,(8.5,19.5)),(Moeda,(8.5,20.5)),(Moeda,(8.5,21.5)),(Moeda,(10.5,21.5)),(Moeda,(10.5,20.5)),(Moeda,(10.5,19.5)),(Moeda,(10.5,18.5)),(Moeda,(11.5,18.5)),(Moeda,(12.5,18.5)),(Moeda,(12.5,19.5)),(Moeda,(12.5,20.5)),(Moeda,(12.5,21.5)),(Moeda,(2.5,16.5)),(Moeda,(3.5,16.5)),(Moeda,(4.5,16.5)),(Moeda,(5.5,16.5)),(Moeda,(6.5,16.5)),(Moeda,(7.5,16.5)),(Moeda,(8.5,16.5)),(Moeda,(9.5,16.5)),(Moeda,(10.5,16.5)),(Moeda,(11.5,16.5)),(Moeda,(12.5,16.5)),(Moeda,(2.5,13.5)),(Moeda,(3.5,13.5)),(Moeda,(4.5,13.5)),(Moeda,(6.5,13.5)),(Moeda,(7.5,13.5)),(Moeda,(8.5,13.5)),(Moeda,(10.5,13.5)),(Moeda,(11.5,13.5)),(Moeda,(12.5,13.5)),(Martelo,(5.5,13.5)),(Escudo,(10.5,7.5))]

jogo6 ::Jogo
jogo6 = Jogo mapa6 inim6 colec6 jog{posicao = posi,direcao=diri}
  where (Mapa (posi,diri) posf blocos) = mapa6



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
    | x == 'L' = Lanca : replaceValue xs
    | otherwise = replaceValue xs



