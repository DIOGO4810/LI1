{-|
Module      : LI12324
Description : Definições base do jogo
Copyright   : Nelson Estevão <d12733@di.uminho.pt>
              Olga Pacheco   <omp@di.uminho.pt>
              Rui Carvalho   <d13696@di.uminho.pt>
              Xavier Pinho   <d12736@di.uminho.pt>

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2023/24.
-}
module LI12324 (
    -- * Tipos de dados
    -- ** Básicos
    Posicao, Velocidade, Tempo, Hitbox, Direcao(..), Semente,
    -- ** Mapas
    Mapa(..), Bloco(..), Personagem(..), Entidade(..), Colecionavel(..),
    -- ** Jogo
    Jogo(..), Acao(..),
    -- * Funções auxiliares fornecidas
    gravidade, geraAleatorios
    ) where

import System.Random (mkStdGen, randoms)

-- | Peças possíveis para construir um 'Mapa'.
data Bloco
  = Escada       -- ^ Permite ao jogador mover-se verticalmente
  | Plataforma   -- ^ Bloco sólido que pode ser utilizado como superfície
  | Alcapao      -- ^ Bloco que desaparece após ser atravessado pelo jogador
  | Vazio        -- ^ Espaço
  deriving (Ord, Eq, Read, Show)

-- | Mapa de um 'Jogo', composto por uma posição e direção inicial, posição final e uma matriz de blocos.
data Mapa =
  Mapa (Posicao, Direcao) Posicao [[Bloco]]
  deriving (Eq, Read, Show)

-- | A caixa de colisão que define o espaço ocupado por um 'Personagem' no 'Mapa' através de um retangulo representativo.
type Hitbox = (Posicao, Posicao)

-- | Vetor velocidade.
type Velocidade = (Double, Double)

-- | Posicao no 'Mapa'.
type Posicao = (Double, Double)

-- | Períodos de tempo.
type Tempo = Double

-- | Direção de um 'Personagem' no 'Mapa'.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  deriving (Ord, Eq, Read, Show)

-- | Tipo de entidades que um 'Personagem' pode tomar.
data Entidade
  = MacacoMalvado
  | Fantasma
  | Jogador
  deriving (Ord, Eq, Read, Show)

-- | Tipos de items passiveis de ser colecionaveis por um 'Personagem'.
data Colecionavel
  = Moeda
  | Martelo
  deriving (Ord, Eq, Read, Show)

-- | Personagem do 'Jogo'.
data Personagem =
  Personagem
    { velocidade :: Velocidade
    , tipo       :: Entidade
    , posicao    :: Posicao
    , direcao    :: Direcao
    , tamanho    :: (Double, Double)
    , emEscada   :: Bool -- ^ se está numa escada
    , ressalta   :: Bool
    , vida       :: Int -- ^ não negativo
    , pontos     :: Int
    , aplicaDano :: (Bool, Double) -- ^ se está armado e por quanto tempo ainda
    }
  deriving (Eq, Read, Show)

-- | A acao tomada por um 'Personagem'.
data Acao
  = Subir
  | Descer
  | AndarDireita
  | AndarEsquerda
  | Saltar
  | Parar
  deriving (Eq, Read, Show)

{- | Vetor velocidade da gravidade.

prop> gravidade == (0, 10)
-}
gravidade :: Velocidade
gravidade = (0, 10)

-- | Definição base de um 'Jogo'.
data Jogo =
  Jogo
    { mapa          :: Mapa -- ^ mapa do jogo
    , inimigos      :: [Personagem] -- ^ lista de inimigos no mapa
    , colecionaveis :: [(Colecionavel, Posicao)] -- ^ lista de colecionaveis espalhados pelo mapa
    , jogador       :: Personagem -- ^ o jogador
    }
  deriving (Eq, Read, Show)

-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

{-| Função que gera uma lista de números aleatórios a partir de uma 'Semente'.

== Exemplos

>>> geraAleatorios 2324 3
[-4152215250714882843,5190394115856197582,1807065739108315696]

>>> geraAleatorios 10 1
[3575835729477015470]
-}
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ randoms (mkStdGen s)
