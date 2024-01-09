{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Diogo José Ribeiro Ribeiro <a106906@alunos.uminho.pt>
              Heitor Araújo Fernandes <a106933@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Niveis
import Utilities
    ( calculaHitbox,
      calculaHitboxDano,
      calculaHitboxDentro,
      calculaHitboxDireita,
      calculaHitboxEmCima,
      calculaHitboxEmbaixo,
      calculaHitboxEsquerda,
      colideComBloco,
      colisaoHitbox,
      getBloco,
      hitboxesBlocos,
      mapaAlcapoes,
      mapaEscadas,
      mapaPlataformas,
      mapaPlataformasAlcapoes,
      mapaVazio,
      replace )
import Tarefa1
import Data.Maybe
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float, int2Double)

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo = ((jogadorWrapper tempo)(inimigosWrapper semente tempo (mapaWrapper jogo)))

jogadorWrapper :: Tempo -> Jogo -> Jogo
jogadorWrapper tempo jogoW = jogoW {
  colecionaveis = tiraColecionaveis (jogador jogoW) (colecionaveis jogoW),
  jogador = movePersonagem tempo $ atrito tempo $ colideEscada (mapa jogoW) $ tiraVidaJogador tempo (mapa jogoW) (inimigos jogoW) $ temporizadorMartelo tempo $ aplicaEfeitos (colecionaveis jogoW) $ stopLimites (mapa jogoW) (inimigos jogoW) $ ejeta (mapa jogoW) tempo $ aplicaGravidade tempo (mapa jogoW) $ fst(isInimigoDead (jogador jogoW) (inimigos jogoW))
}

inimigosWrapper :: Semente -> Tempo -> Jogo -> Jogo
inimigosWrapper semente tempo jogoW = jogoW {
  --inimigos = isInimigoDead $ tiraVidaInimigos (jogador jogoW) $ map (\i -> colideEscada $ stopLimites (mapa jogoW) $ aplicaGravidade i) (inimigos jogoW)
  inimigos = snd $ isInimigoDead (jogador jogoW) $ tiraVidaInimigos (jogador jogoW) $ aplicaMovimento semente (mapa jogoW) $ map (\i -> movePersonagem tempo $ controlaInimigo (mapa jogoW) $ paraSubirDescer (mapa jogoW) i) (inimigos jogoW)
}

mapaWrapper :: Jogo -> Jogo
mapaWrapper jogoW = jogoW {
  mapa = tiraAlcapoes (jogador jogoW) (mapa jogoW)
}

-- | 1. Função que retira 1 vida aos inimigos se eles colidirem com a hitbox de dano de um jogador armado
tiraVidaInimigos :: Personagem -> [Personagem] -> [Personagem]
tiraVidaInimigos jogador  listaInimigos = 
  if isArmado && tempoArmado > 0 
    then map (\personagem -> if ((colisaoHitbox (calculaHitboxDano jogador) (calculaHitbox personagem))) then (personagem{vida=vida personagem-1}) else personagem) (listaInimigos)
  else listaInimigos
  where (isArmado, tempoArmado) = aplicaDano jogador

-- | 2. Função que faz os inimigos desaparecer do mapa quando perdem todas as vidas
isInimigoDead :: Personagem -> [Personagem] -> (Personagem,[Personagem])
isInimigoDead jogador listaInimigos =
  if isDead
    then (jogador{pontos = pontos jogador + 200},map (\personagem -> if vida personagem == 0 then personagem {vida=0, posicao = (-100,-100)} else personagem) listaInimigos)
  else (jogador,listaInimigos)
  where isDead = any (\personagem -> vida personagem==0 && posicao personagem /= (-100,-100)) listaInimigos

-- | 3. Função que aplica um efeito de gravidade aos personagens se eles não se encontrarem numa plataforma ou num alçapão
aplicaGravidade :: Tempo -> Mapa -> Personagem -> Personagem
aplicaGravidade tempo mapa personagem =
  if (emEscada personagem == False && not((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa)) && ((snd $ velocidade personagem) >= 3) && (snd $ velocidade personagem) < 4)) && (any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && (snd $ velocidade personagem)>=0 
    then personagem {velocidade=(fst $ velocidade personagem,0)}
  else if emEscada personagem == False
    then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
  else if emEscada personagem && (fst $ velocidade personagem) /= 0 && not (any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas (mapa))))
    then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
  else personagem {velocidade=(fst $ velocidade personagem, snd $ velocidade personagem)}
  where (Mapa _ _ blocos) = mapa

-- | 4. Função que retira uma vida e aplica um efeito de bounce back ao jogador quando ele colide com um inimigo e está desarmado
-- TODO: Bounce back nas escadas
tiraVidaJogador :: Tempo -> Mapa -> [Personagem] -> Personagem -> Personagem
tiraVidaJogador tempo mapa listaInimigos personagem = 
  -- Colisão Direita dentro do mapa
  if any (\inimigo -> colisaoHitbox (calculaHitboxDireita personagem) (calculaHitbox inimigo)) listaInimigos && ((fst $ velocidade personagem) /= -4.5 && (fst $ velocidade personagem) /= 4.5) && ((px - tamanhoX/2) > tamanhoX) && ((px + tamanhoX/2) < fromIntegral(length (head blocos)) - tamanhoX)
    then personagem{vida=vida personagem-1, velocidade=(-4.5,(-4)+(snd gravidade)*tempo)} 
  -- Colisão Esquerda dentro do mapa
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEsquerda personagem) (calculaHitbox inimigo)) listaInimigos && ((fst $ velocidade personagem) /= -4.5 && (fst $ velocidade personagem) /= 4.5) && ((px - tamanhoX/2) > tamanhoX) && ((px + tamanhoX/2) < fromIntegral(length (head blocos)) - tamanhoX)
    then personagem{vida=vida personagem-1, velocidade=(4.5,(-4)+(snd gravidade)*tempo)}  
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && any (\hitbox -> colisaoHitbox (calculaHitboxEmCima personagem) hitbox) (hitboxesBlocos(mapaPlataformas mapa)) && (snd $ velocidade personagem) <= 0 && emEscada personagem
    then personagem{vida=vida personagem-1, velocidade=(0,4)}  
  -- Colisão Emcima
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && any (\hitbox -> colisaoHitbox (calculaHitboxEmCima personagem) hitbox) (hitboxesBlocos(mapaPlataformas mapa)) && (snd $ velocidade personagem) <= 0 && emEscada personagem
    then personagem{vida=vida personagem-1, velocidade=(0,4)}  
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && any (\pos -> (fromIntegral(floor px),fromIntegral(ceiling py)) == pos) (mapaPlataformas mapa) && (snd $ velocidade personagem) >= 0 && (fst $ velocidade personagem)==0 && emEscada personagem
    then personagem{vida=vida personagem-1, velocidade=(-4.5,-4+(snd gravidade)*tempo)}  
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && (fst $ velocidade personagem)==0 && emEscada personagem
    then personagem{vida=vida personagem-1, velocidade=(-4.5,4-(snd gravidade)*tempo)}  
  --Colisão Embaixo
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmbaixo personagem) (calculaHitbox inimigo)) listaInimigos && (snd $ velocidade personagem) >= 0 && emEscada personagem
    then personagem{vida=vida personagem-1, velocidade=(-4.5,-4+(snd gravidade)*tempo)} 
  --Colisão Limite Esquerdo do mapa
  else if any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && (snd $ velocidade personagem) >= 0 && ((px - tamanhoX/2) < tamanhoX)
    then personagem{vida=vida personagem-1, velocidade=(4.5,(-4)+(snd gravidade)*tempo)} 
  -- Colisão Limite Direito do mapa
  else if any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && (snd $ velocidade personagem) >= 0 && ((px + tamanhoX/2) > fromIntegral(length (head blocos)) - tamanhoX)
    then personagem{vida=vida personagem-1, velocidade=(-4.5,(-4)+(snd gravidade)*tempo)} 
  else personagem
  where 
    (px,py) = posicao personagem
    (tamanhoX,tamanhoY) = tamanho personagem
    (Mapa _ _ blocos) = mapa
    
-- | 5. Função que retira os colecionáveis quando são coletados e aplica os seus efeitos
aplicaEfeitos :: [(Colecionavel, Posicao)] -> Personagem -> Personagem
aplicaEfeitos listaColecionaveis personagem = foldl(\personagem (col,(x,y)) -> if (colisaoHitbox (calculaHitbox personagem) ((x-0.5,y+0.5),(x+0.5,y-0.5))) then (if col == Moeda then personagem {pontos=(pontos personagem)+100} else personagem {aplicaDano=(True,10)}) else personagem) personagem listaColecionaveis

tiraColecionaveis :: Personagem -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
tiraColecionaveis jogadorJogo listaColecionaveis = filter (\(col,(x,y)) -> not (colisaoHitbox (calculaHitbox jogadorJogo) ((x-0.5,y+0.5),(x+0.5,y-0.5)))) listaColecionaveis

temporizadorMartelo :: Tempo -> Personagem -> Personagem
temporizadorMartelo tempo jogador =
  if tempoArmado > 0 
    then jogador {aplicaDano = (True,tempoArmado-tempo)}
  else if tempoArmado <= 0
    then jogador {aplicaDano = (False,0)}
  else jogador
  where (isArmado,tempoArmado) = aplicaDano jogador


-- | 6. Função que faz um alcapão desaparecer depois do jogador o pisar
tiraAlcapoes :: Personagem -> Mapa -> Mapa
tiraAlcapoes jogadorJogo (Mapa ((xi,yi),dir) (xf,yf) blocos) =
  if (any (\hitboxalcapao -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxalcapao) hitboxesalcapoes) && (getBloco (px+0.6,py+1) blocos == Alcapao) && (direcao jogadorJogo) == Oeste
    then (Mapa ((xi,yi),dir) (xf,yf) (replace blocos (px+0.6,py)))
  else if (any (\hitboxalcapao -> colisaoHitbox (calculaHitbox jogadorJogo) hitboxalcapao) hitboxesalcapoes) && (getBloco (px-0.6,py+1) blocos == Alcapao) && (direcao jogadorJogo) == Este
    then (Mapa ((xi,yi),dir) (xf,yf) (replace blocos (px-0.6,py)))
  else (Mapa ((xi,yi),dir) (xf,yf) blocos)
  where 
    posalcapoes = mapaAlcapoes (Mapa ((xi,yi),dir) (xf,yf) blocos)
    hitboxesalcapoes = hitboxesBlocos posalcapoes
    (px,py) = posicao jogadorJogo

-- | 7. Função que impede os personagens de sair dos limites do mapa e atravessar plataformas
stopLimites :: Mapa -> [Personagem] -> Personagem -> Personagem
stopLimites mapa listaInimigos personagem= 
  if (((px + tamanhoX/2) >= fromIntegral(length (head blocos))) || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) > 0
    then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Oeste}
  else if ((px - tamanhoX/2) <= 0 || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) < 0
    then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Este}
  else if emEscada personagem && colideComBloco personagem mapa && (snd $ velocidade personagem)>0 && not(any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa)) 
    then personagem {velocidade = (0,0)}
  else if py - tamanhoY / 2 <= 0
    then personagem {velocidade = (fst $ velocidade personagem,10)}
  else personagem
  where (px,py) = posicao personagem
        (tamanhoX, tamanhoY) = tamanho personagem
        (Mapa a b blocos) = mapa

-- | Função que verifica se um personagem está numa escada
colideEscada :: Mapa -> Personagem -> Personagem
colideEscada mapa personagem= 
  if any (\(xe,ye) -> fromIntegral(floor $ fst $ posicao personagem) == xe && fromIntegral(floor $ snd $ posicao personagem) == ye) (mapaEscadas mapa) || ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa)) && (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))))
    then personagem{emEscada=True}
  else personagem {emEscada = False}

-- | Função que o faz o jogador mover-se
movePersonagem :: Tempo -> Personagem  -> Personagem
movePersonagem tempo personagem= personagem {posicao = (px+(fst $ velocidade personagem)*tempo,py+(snd $ velocidade personagem)*tempo)}
  where (px,py) = posicao personagem

-- | Função que controla a direção dos inimigos
controlaInimigo :: Mapa -> Personagem -> Personagem
controlaInimigo mapa inimigo = 
  if ((any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) || (px + tamanhoX/2) >= fromIntegral(length (head blocos)) || any (\(x,y) -> (fromIntegral(ceiling px-1),fromIntegral(ceiling py)) == (x,y)) (mapaVazio mapa))) && (fst $ velocidade inimigo) > 0
      then inimigo {velocidade = (-1.5, 0), direcao = Oeste}
  else if ((any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) || (px - tamanhoX/2) <= 0 || any (\(x,y) -> (fromIntegral(floor px),fromIntegral(ceiling py)) == (x,y)) (mapaVazio mapa))) && (fst $ velocidade inimigo) < 0
    then inimigo {velocidade = (1.5, 0), direcao = Este}
  else inimigo
  where 
    (px,py) = posicao inimigo
    (tamanhoX,tamanhoY) = tamanho inimigo
    (Mapa _ _ blocos) = mapa

-- | Função que escolhe aleatoriamente a trajetória de um inimigo
iniciaMovimento :: Mapa -> (Personagem, Int) -> Personagem
iniciaMovimento mapa (inim, int) =
  if velocidade inim == (0,0)
    then if even int 
          then inim{velocidade=(1.5,0),direcao=Este} 
         else inim{velocidade=(-1.5,0),direcao=Oeste}
  else if podeSubir mapa inim && (any (\posesc-> (px > (fst posesc)+0.495) && (px < (fst posesc)+0.505)) (mapaEscadas mapa)) && (snd $ velocidade inim) == 0 && mod int 2 == 0
    then inim{velocidade=(0,-1.5),direcao=Norte}
  else if podeDescer mapa inim && (any (\posesc-> ((fst $ posicao inim) > (fst posesc)+0.495) && ((fst $ posicao inim) < (fst posesc)+0.505)) (mapaEscadas mapa)) && (snd $ velocidade inim) == 0 && mod int 2 == 0
    then inim{velocidade=(0,1.5),direcao=Sul}
  else inim
  where (px,py) = posicao inim

-- | Função que aplica a função iniciaMovimento juntamente com a semente à lista de inimigos
aplicaMovimento :: Semente -> Mapa -> [Personagem] -> [Personagem]
aplicaMovimento s mapa inimigos = map (iniciaMovimento mapa) inimint
  where inimint = zip inimigos (geraAleatorios s (length inimigos))

podeDescer :: Mapa -> Personagem -> Bool
podeDescer mapa inimigo =  ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos((mapaEscadas mapa)))) && not((any (\posplat -> (fromIntegral(floor px),fromIntegral(ceiling (py-0.5))) == posplat) (mapaPlataformas mapa)))) || (((any (\posplat -> (fromIntegral(floor px),fromIntegral(ceiling (py))) == posplat) (mapaPlataformas mapa)) && (any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+1))) == escadabaixo) (mapaEscadas (mapa))))) || (((any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos((mapaPlataformas mapa)))) && (any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+1))) == escadabaixo) (mapaEscadas (mapa)))))  
 where (px,py) = posicao inimigo

podeSubir :: Mapa -> Personagem -> Bool
podeSubir mapa inimigo =  any (\(xe,ye) -> fromIntegral(floor $ fst $ posicao inimigo) == xe && fromIntegral(floor $ snd $ posicao inimigo) == ye) (mapaEscadas mapa) || ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+2))) == escadabaixo) (mapaEscadas (mapa)))
 where (px,py) = posicao inimigo

paraSubirDescer :: Mapa -> Personagem -> Personagem
paraSubirDescer mapa inimigo = 
  if (snd $ velocidade inimigo) > 0 && not(podeDescer mapa inimigo)
    then inimigo{velocidade =((fst $ velocidade inimigo),0)}
  else if (snd $ velocidade inimigo) < 0 && not(podeSubir mapa inimigo)
    then inimigo{velocidade =((fst $ velocidade inimigo),0)}
  else inimigo

-- | Função que ejeta o jogador se ele ficar preso num bloco
ejeta :: Mapa -> Tempo -> Personagem -> Personagem
ejeta mapa tempo personagem = 
  if not (emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) && not((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa))) 
    then personagem {velocidade = (fst $ (velocidade personagem), -3)}
  else if (emEscada personagem) && direcao personagem /= Norte && direcao personagem /= Sul && any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) 
    then personagem {velocidade = (fst $ (velocidade personagem), -3)}  
  else if not (emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmCima personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) 
    then personagem {velocidade = (fst $ (velocidade personagem), 1+(snd $ gravidade)*tempo)}
  else personagem
  where (Mapa _ _ blocos) = mapa

-- | Função que aplica atrito ao jogador
atrito :: Tempo -> Personagem -> Personagem
atrito tempo personagem =
  if (fst $ velocidade personagem) /= 4 && (fst $ velocidade personagem) > 1  && (snd $ velocidade personagem) == 0
    then personagem {velocidade = (((fst $ velocidade personagem) - 30*tempo), (snd $ velocidade personagem))}
  else if (fst $ velocidade personagem) /= -4 && (fst $ velocidade personagem) < -1 && (snd $ velocidade personagem) == 0
    then personagem {velocidade = (((fst $ velocidade personagem) + 30*tempo), (snd $ velocidade personagem))}
  else if (fst $ velocidade personagem) > -1 && (fst $ velocidade personagem) /= 0 && (fst $ velocidade personagem) <1 && (snd $ velocidade personagem) == 0
    then personagem {velocidade = (0, (snd $ velocidade personagem))}
  else personagem


