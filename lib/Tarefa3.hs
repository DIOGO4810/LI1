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
import Tarefa1
import Data.Maybe
import Graphics.Gloss.Data.Point (pointInBox)
import GHC.Float (double2Float, int2Double)

-- | Função que recebe uma semente, um tempo e um jogo. Devolve um novo jogo com as modificações de movimento aplicadas

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo = ((jogadorWrapper tempo)(inimigosWrapper semente tempo (mapaWrapper jogo)))

-- | Conjunto de funções referentes ao jogador e aos colecionáveis

jogadorWrapper :: Tempo -> Jogo -> Jogo
jogadorWrapper tempo jogoW = jogoW {
  colecionaveis = tiraColecionaveis (jogador jogoW) (colecionaveis jogoW),
  jogador = movePersonagem tempo $ atrito tempo $ colideEscada (mapa jogoW) $ tiraVidaJogador tempo (mapa jogoW) (inimigos jogoW) $ temporizadorEscudo tempo $ temporizadorKickback tempo $ temporizadorMartelo tempo $ aplicaEfeitos (colecionaveis jogoW) $ stopLimites (mapa jogoW)$ bounce (mapa jogoW) tempo $ atualizaImpulsao (mapa jogoW) $ ejeta (mapa jogoW) tempo $ aplicaGravidade tempo (mapa jogoW) (jogador jogoW) 
}

-- | Conjunto de funções referentes aos inimigos

inimigosWrapper :: Semente -> Tempo -> Jogo -> Jogo
inimigosWrapper semente tempo jogoW = jogoW {
  inimigos = isInimigoDead $ tiraVidaInimigos (jogador jogoW) $ retornaBarris (mapa jogoW) $ lancaBarris (jogador jogoW) $ temporizadorBarris tempo $ aplicaMovimento semente (mapa jogoW) $ map (\i -> movePersonagem tempo $ controlaInimigo (mapa jogoW) $ paraSubirDescer (mapa jogoW) i) (inimigos jogoW)
}

-- | Conjunto de funções referentes ao mapa

mapaWrapper :: Jogo -> Jogo
mapaWrapper jogoW = jogoW {
  mapa = tiraAlcapoes (jogador jogoW) (mapa jogoW)
}

-- | 1. Função que retira 1 vida aos inimigos se eles colidirem com a hitbox de dano de um jogador armado
tiraVidaInimigos :: Personagem -> [Personagem] -> [Personagem]
tiraVidaInimigos jogador listaInimigos = 
  if isArmado && tempoArmado > 0 && not(emEscada jogador)
    then map (\personagem -> if ((colisaoHitbox (calculaHitboxDano jogador) (calculaHitbox personagem))) && tipo personagem == Fantasma then (personagem{vida=vida personagem-1}) else personagem) (listaInimigos)
  else listaInimigos
  where (isArmado, tempoArmado) = aplicaDano jogador

-- | 2. Função que faz os inimigos desaparecerem do mapa quando perdem todas as vidas
isInimigoDead :: [Personagem] -> [Personagem]
isInimigoDead listaInimigos = map (\personagem -> if vida personagem == 0 then personagem {posicao = (-100,-100)} else personagem) listaInimigos

-- | 3. Função que aplica um efeito de gravidade aos personagens se eles não se encontrarem numa plataforma ou num alçapão
aplicaGravidade :: Tempo -> Mapa -> Personagem -> Personagem
aplicaGravidade tempo mapa personagem =
  if (not(emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && (snd $ velocidade personagem) >= 0 && (snd $ velocidade personagem) /= 3
    then personagem {velocidade=(fst $ velocidade personagem,0)}
  else if not(emEscada personagem) && not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && not(any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem))) == pos) (mapaEscadas mapa))
    then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
  else if emEscada personagem && (fst $ velocidade personagem) /= 0 && not (any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas (mapa))))
    then personagem {velocidade=(fst $ velocidade personagem, (snd $ velocidade personagem) + (snd gravidade)*tempo)}
  else personagem
  where (Mapa _ _ blocos) = mapa

-- | 4. Função que retira uma vida e aplica um efeito de bounce back ao jogador quando ele colide com um inimigo ou com uma lança e está desarmado
tiraVidaJogador :: Tempo -> Mapa -> [Personagem] -> Personagem -> Personagem
tiraVidaJogador tempo mapa listaInimigos personagem = 
  -- Colisão à beira de um muro
  if ((any (\pos -> (fromIntegral(floor((fst $ posicao personagem) - (tamanhoX/2+0.8))), fromIntegral(floor $ snd $ posicao personagem)) == pos) (mapaPlataformas mapa))) && any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(4.5,(-4)+(snd gravidade)*tempo)}  
  else if (any (\pos -> (fromIntegral(floor((fst $ posicao personagem) + (tamanhoX/2+0.8))), fromIntegral(floor $ snd $ posicao personagem)) == pos) (mapaPlataformas mapa)) && any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,(-4)+(snd gravidade)*tempo)}
  -- Colisão á direita dentro do mapa
  else if (any (\inimigo -> colisaoHitbox (calculaHitboxDireita personagem) (calculaHitbox inimigo)) listaInimigos && ((fst $ velocidade personagem) /= -4.5 && (fst $ velocidade personagem) /= 4.5) && ((px - tamanhoX/2) > tamanhoX) && ((px + tamanhoX/2) < fromIntegral(length (head blocos)) - tamanhoX) && not((any (\pos -> (fromIntegral(floor((fst $ posicao personagem) - (tamanhoX/2+0.8))), fromIntegral(floor $ snd $ posicao personagem)) == pos) (mapaPlataformas mapa))) || any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa))) && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,(-4)+(snd gravidade)*tempo)} 
  else if any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa)) && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=((-(fst $ velocidade personagem))*1.125,-4+(snd gravidade)*tempo)}
  -- Colisão á esquerda dentro do mapa
  else if (any (\inimigo -> colisaoHitbox (calculaHitboxEsquerda personagem) (calculaHitbox inimigo)) listaInimigos && ((fst $ velocidade personagem) /= -4.5 && (fst $ velocidade personagem) /= 4.5) && ((px - tamanhoX/2) > tamanhoX) && ((px + tamanhoX/2) < fromIntegral(length (head blocos)) - tamanhoX) && not(any (\pos -> (fromIntegral(floor((fst $ posicao personagem) + (tamanhoX/2+0.8))), fromIntegral(floor $ snd $ posicao personagem)) == pos) (mapaPlataformas mapa)) || any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa))) && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(4.5,(-4)+(snd gravidade)*tempo)}  
  else if any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa)) && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=((-(fst $ velocidade personagem))*1.125,-4+(snd gravidade)*tempo)}
  -- Colisão em cima
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && any (\hitbox -> colisaoHitbox (calculaHitboxEmCima personagem) hitbox) (hitboxesBlocos(mapaPlataformas mapa)) && (snd $ velocidade personagem) <= 0 && emEscada personagem && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(0,4)}  
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && any (\pos -> (fromIntegral(floor px),fromIntegral(ceiling py)) == pos) (mapaPlataformas mapa) && (snd $ velocidade personagem) >= 0 && (fst $ velocidade personagem)==0 && emEscada personagem && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,-4+(snd gravidade)*tempo)}  
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmCima personagem) (calculaHitbox inimigo)) listaInimigos && (fst $ velocidade personagem)==0 && (snd $ velocidade personagem) <= 0 && emEscada personagem && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,4-(snd gravidade)*tempo)}  
  --Colisão em baixo
  else if any (\inimigo -> colisaoHitbox (calculaHitboxEmbaixo personagem) (calculaHitbox inimigo)) listaInimigos && (snd $ velocidade personagem) >= 0 && emEscada personagem && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,-4+(snd gravidade)*tempo)} 
  else if any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa)) && (not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)))) && (not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaTrampolins mapa)))) && (fst $ velocidade personagem) /= 0 && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=((-(fst $ velocidade personagem))*1.125,-4+(snd gravidade)*tempo)}
  else if any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaLancas mapa)) && (not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)))) && (not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaTrampolins mapa)))) && (fst $ velocidade personagem) == 0 && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=((fst $ velocidade personagem),-6+(snd gravidade)*tempo)}
  --Colisão no limite esquerdo do mapa
  else if ((px - tamanhoX/2) < tamanhoX) && any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(4.5,(-4)+(snd gravidade)*tempo)} 
  -- Colisão no limite direito do mapa
  else if ((px + tamanhoX/2) > fromIntegral(length (head blocos)) - tamanhoX) && any (\inimigo -> colisoesPersonagens personagem inimigo) listaInimigos && not(fst $ escudo personagem) && not(fst $ kickback personagem)
    then personagem{kickback=(True,1),vida=vida personagem-1, velocidade=(-4.5,(-4)+(snd gravidade)*tempo)} 
  else personagem
  where 
    (px,py) = posicao personagem
    (tamanhoX,tamanhoY) = tamanho personagem
    (Mapa _ _ blocos) = mapa
    


-- | Função que ativa o kickback do personagem fazendo com que o mesmo na tipagem tenha o primeiro argumento em True caso o segundo seja maior que 0 sabendo que o segundo elemento diminui com o tempo

temporizadorKickback :: Tempo -> Personagem -> Personagem
temporizadorKickback tempo jogador =
  if tempoKickback > 0 
    then jogador{kickback = (True,tempoKickback-tempo)}
  else if tempoKickback <= 0
    then jogador {kickback = (False,0)}
  else jogador
  where (hasKickback,tempoKickback) = kickback jogador

-- | 5. Função que retira os colecionáveis quando são coletados e aplica os seus efeitos
aplicaEfeitos :: [(Colecionavel, Posicao)] -> Personagem -> Personagem
aplicaEfeitos listaColecionaveis personagem = foldl(\personagem (col,(x,y)) -> if (colisaoHitbox (calculaHitbox personagem) ((x-0.5,y+0.5),(x+0.5,y-0.5))) then (if col == Moeda then personagem {pontos=(pontos personagem)+100} else if col==Martelo then personagem {aplicaDano=(True,10)} else personagem {escudo=(True,5)}) else personagem) personagem listaColecionaveis


-- | Função que retira o colecionavel da lista de colecionaveis caso o hitbox do mesmo entre em contacto com o do player
tiraColecionaveis :: Personagem -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
tiraColecionaveis jogadorJogo listaColecionaveis = filter (\(col,(x,y)) -> not (colisaoHitbox (calculaHitbox jogadorJogo) ((x-0.5,y+0.5),(x+0.5,y-0.5)))) listaColecionaveis

-- | Função que ativa o martelo fazendo com que o mesmo na tipagem tenha o primeiro argumento em True caso o segundo seja maior que 0 sabendo que o segundo elemento diminui com o tempo
temporizadorMartelo :: Tempo -> Personagem -> Personagem
temporizadorMartelo tempo jogador =
  if tempoArmado > 0 
    then jogador {aplicaDano = (True,tempoArmado-tempo)}
  else if tempoArmado <= 0
    then jogador {aplicaDano = (False,0)}
  else jogador
  where (isArmado,tempoArmado) = aplicaDano jogador

-- | Função que ativa o escudo fazendo com que o mesmo na tipagem tenha o primeiro argumento em True caso o segundo seja maior que 0 sabendo que o segundo elemento diminui com o tempo
temporizadorEscudo :: Tempo -> Personagem -> Personagem
temporizadorEscudo tempo jogador =
  if tempoEscudo > 0 
    then jogador {escudo = (True,tempoEscudo-tempo)}
  else if tempoEscudo <= 0
    then jogador {escudo = (False,0)}
  else jogador
  where (hasEscudo,tempoEscudo) = escudo jogador


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
stopLimites :: Mapa -> Personagem -> Personagem
stopLimites mapa personagem= 
  if (((px + tamanhoX/2) >= fromIntegral(length (head blocos))) || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) > 0
    then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Oeste}
  else if ((px - tamanhoX/2) <= 0 || (any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda personagem) hitboxbloco) (hitboxesBlocos (mapaPlataformasAlcapoes blocos)))) && (fst $ (velocidade personagem)) < 0
    then personagem {velocidade = (0, snd $ velocidade personagem), direcao = Este}
  else if emEscada personagem && (snd $ velocidade personagem)>0 && (any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitbox) (hitboxesBlocos(mapaPlataformas mapa)) && not((any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 0.9)) == pos) (mapaEscadas mapa)) || (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem))) == pos) (mapaEscadas mapa))))
    then personagem {velocidade = (0,0)}
  else if emEscada personagem && (snd $ velocidade personagem) /= 0 && (snd $ velocidade personagem) /= -3 && (snd $ velocidade personagem) /= 3 && ((fst $ velocidade personagem) == 0 || impulsao personagem) && not (any (\hitbox -> colisaoHitbox (calculaHitboxDentro personagem) hitbox) (hitboxesBlocos(mapaPlataformas mapa)))
    then personagem {velocidade = (0,0),direcao=Norte}
  else if py - tamanhoY / 2 <= 0
    then personagem {velocidade = (fst $ velocidade personagem, 1)}
  else personagem
  where (px,py) = posicao personagem
        (tamanhoX, tamanhoY) = tamanho personagem
        (Mapa a b blocos) = mapa

-- | Função que verifica se um personagem está numa escada
colideEscada :: Mapa -> Personagem -> Personagem
colideEscada mapa personagem= 
  if any (\(xe,ye) -> fromIntegral(floor $ fst $ posicao personagem) == xe && fromIntegral(floor $ snd $ posicao personagem) == ye) (mapaEscadas mapa) || ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && ((any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa)) || (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem))) == pos) (mapaEscadas mapa))) && (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))))
    then personagem{emEscada=True}
  else personagem {emEscada = False}

-- | Função que o faz o jogador mover-se
movePersonagem :: Tempo -> Personagem -> Personagem
movePersonagem tempo personagem= personagem {posicao = (px+(fst $ velocidade personagem)*tempo,py+(snd $ velocidade personagem)*tempo)}
  where (px,py) = posicao personagem

-- | Função que controla a direção dos inimigos
controlaInimigo :: Mapa -> Personagem -> Personagem
controlaInimigo mapa inimigo = 
  if not(any (\hitboxblocos-> colisaoHitbox (calculaHitboxEmbaixo inimigo) hitboxblocos) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && any (\hitboxblocos-> colisaoHitbox (calculaHitboxEmbaixo inimigo) hitboxblocos) (hitboxesBlocos(mapaVazio mapa)) && tipo inimigo == Fantasma && pxn>px && snd (velocidade inimigo) == 0
    then inimigo {velocidade = (1.5, 0),posicao = (pxn+0.5,pyn), direcao = Este}
  else if not(any (\hitboxblocos-> colisaoHitbox (calculaHitboxEmbaixo inimigo) hitboxblocos) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))) && any (\hitboxblocos-> colisaoHitbox (calculaHitboxEmbaixo inimigo) hitboxblocos) (hitboxesBlocos(mapaVazio mapa)) && tipo inimigo == Fantasma && pxn<px && snd (velocidade inimigo) == 0
    then inimigo {velocidade = (-1.5, 0),posicao = (pxn,pyn), direcao = Oeste}
  else if (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDireita inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) || (px + tamanhoX/2) >= fromIntegral(length (head blocos)) || (any (\hitboxblocos-> colisaoHitbox (calculaHitboxInfDireita inimigo) hitboxblocos) (hitboxesBlocos(mapaVazio mapa)) && not(any (\hitboxblocos-> colisaoHitbox (calculaHitboxInfDireita inimigo) hitboxblocos) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))))) && (fst $ velocidade inimigo) > 0 && snd (velocidade inimigo) == 0 && tipo inimigo == Fantasma
      then inimigo {velocidade = (-1.5, 0), direcao = Oeste}
  else if (any (\hitboxbloco -> colisaoHitbox (calculaHitboxEsquerda inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) || (px - tamanhoX/2) <= 0 || (any (\hitboxblocos-> colisaoHitbox (calculaHitboxInfEsquerda inimigo) hitboxblocos) (hitboxesBlocos(mapaVazio mapa)) && not(any (\hitboxblocos-> colisaoHitbox (calculaHitboxInfEsquerda inimigo) hitboxblocos) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))))) && (fst $ velocidade inimigo) < 0 && snd (velocidade inimigo) == 0 && tipo inimigo == Fantasma
    then inimigo {velocidade = (1.5, 0), direcao = Este}
  else inimigo 
  where 
    (px,py) = posicao inimigo
    (pxn,pyn) = (fst(plataformaProxima (px,py) mapa), snd(plataformaProxima (px,py) mapa)-0.5)
    (tamanhoX,tamanhoY) = tamanho inimigo
    (Mapa _ _ blocos) = mapa

-- | Função que escolhe aleatoriamente a trajetória de um inimigo
iniciaMovimento :: Mapa -> (Personagem, Int) -> Personagem
iniciaMovimento mapa (inim, int) =
  if podeDescer mapa inim && (any (\posesc-> (px > (fst posesc)+0.495) && (px < (fst posesc)+0.505)) (mapaEscadas mapa)) && (snd $ velocidade inim) == 0 && mod int 6 == 0
    then inim{velocidade=(0,1.5),direcao=Sul}
  else if podeSubir mapa inim && (any (\posesc-> (px > (fst posesc)+0.495) && (px < (fst posesc)+0.505)) (mapaEscadas mapa)) && (snd $ velocidade inim) == 0 && mod int 6 == 0
    then inim{velocidade=(0,-1.5),direcao=Norte}
  else if velocidade inim == (0,0)
    then if even int 
          then inim{velocidade=(1.5,0),direcao=Este} 
        else inim{velocidade=(-1.5,0),direcao=Oeste}
  else inim
  where (px,py) = posicao inim

-- | Função que aplica a função "iniciaMovimento" juntamente com a semente à lista de inimigos
aplicaMovimento :: Semente -> Mapa -> [Personagem] -> [Personagem]
aplicaMovimento s mapa inimigos = map (\(inim,int) -> if tipo inim == Fantasma then iniciaMovimento mapa (inim,int) else inim) inimint
  where inimint = zip inimigos (geraAleatorios s (length inimigos))

-- | Funções que controlam a subida e descida de escadas dos inimigos
podeDescer :: Mapa -> Personagem -> Bool
podeDescer mapa inimigo = ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos((mapaEscadas mapa)))) && not((any (\posplat -> (fromIntegral(floor px),fromIntegral(ceiling (py-0.4))) == posplat) (mapaPlataformas mapa)))) || ((any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo inimigo) hitboxbloco) (hitboxesBlocos((mapaPlataformas mapa)))) && (any (\escadabaixo -> (fromIntegral(floor px),fromIntegral(ceiling (py+1))) == escadabaixo) (mapaEscadas (mapa))))
 where (px,py) = posicao inimigo

podeSubir :: Mapa -> Personagem -> Bool
podeSubir mapa inimigo =  ((any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos((mapaEscadas mapa)))) || (any (\hitboxbloco -> colisaoHitbox (calculaHitbox inimigo) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa)))) && not((any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmCima inimigo) hitboxbloco) (hitboxesBlocos((mapaEscadas mapa)))) && (any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro inimigo) hitboxbloco) (hitboxesBlocos((mapaVazio mapa)))))
 where (px,py) = posicao inimigo

paraSubirDescer :: Mapa -> Personagem -> Personagem
paraSubirDescer mapa inimigo = 
  if (snd $ velocidade inimigo) > 0 && not(podeDescer mapa inimigo) && tipo inimigo == Fantasma
    then inimigo{posicao = ((fst $ posicao inimigo),fromIntegral(floor(snd $ posicao inimigo))+0.5), velocidade = ((fst $ velocidade inimigo),0)}
  else if (snd $ velocidade inimigo) < 0 && not(podeSubir mapa inimigo) && tipo inimigo == Fantasma
    then inimigo{posicao = ((fst $ posicao inimigo),fromIntegral(ceiling(snd $ posicao inimigo))-0.5), velocidade = ((fst $ velocidade inimigo),0)}
  else inimigo

-- | Funções que faz o macaco lançar barris, temporizar a duração dos lançamentos e receção de um novo barril após lançamento do anterior
lancaBarris :: Personagem -> [Personagem] -> [Personagem]
lancaBarris jogador listaInimigos = 
  if any (\i ->tipo i == MacacoMalvado && not(fst $ aplicaDano i)) listaInimigos
   then map (\i -> if tipo i == Barril then i{velocidade=((fst(posicao jogador)-fst(posicao i))/2.5,(snd(posicao jogador)-snd(posicao i))/2.5)} else if tipo i == MacacoMalvado then i{aplicaDano=(True,10.0)} else i) listaInimigos
  else listaInimigos

temporizadorBarris :: Tempo -> [Personagem] -> [Personagem]
temporizadorBarris tempo listaInimigos = map (\i -> if tipo i == MacacoMalvado && (snd $ aplicaDano i) > 0 then i{aplicaDano = (True,(snd $ aplicaDano i)-tempo)} else if (snd $ aplicaDano i) <= 0 then i{aplicaDano = (False,0)} else i) listaInimigos 

retornaBarris :: Mapa -> [Personagem] -> [Personagem]
retornaBarris (Mapa _ _ blocos) listaInimigos=map (\i -> if tipo i == Barril && ((fst $ posicao i)<0 || (fst $ posicao i)>fromIntegral(length(head blocos)) || (snd $ posicao i)<0 || (snd $ posicao i)> fromIntegral(length blocos) || all (\i -> (snd $ aplicaDano i)<0.1) listaInimigos) then i{posicao=(px,py-1.3),velocidade=(0,0)} else i) listaInimigos
  where (px,py) = foldl (\(px,py) i -> if tipo i == MacacoMalvado then (posicao i) else (px,py)) (0,0) listaInimigos

-- | Função que ejeta o jogador se ele ficar preso num bloco
ejeta :: Mapa -> Tempo -> Personagem -> Personagem
ejeta mapa tempo personagem = 
  if not(impulsao personagem) && not (emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaAlcapoes mapa)) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) && not((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa))) 
    then personagem {velocidade = (fst $ (velocidade personagem), -2)} 
  else if not (emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa)) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmbaixo personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) && not((any (\hitboxbloco -> colisaoHitbox (calculaHitbox personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformas mapa))) && (any (\pos -> (fromIntegral $ floor (fst $ posicao personagem),fromIntegral $ ceiling ((snd $ posicao personagem) + 1)) == pos) (mapaEscadas mapa))) 
    then personagem {velocidade = (fst $ (velocidade personagem), -2)} 
  else if not (emEscada personagem) && any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmCima personagem) hitboxbloco) (hitboxesBlocos(mapaPlataformasAlcapoes blocos)) 
    then personagem {velocidade = (fst $ (velocidade personagem), 1+(snd $ gravidade)*tempo)}
  else if any (\hitboxbloco -> colisaoHitbox (calculaHitboxDentro personagem) hitboxbloco) (hitboxesBlocos(mapaTrampolins mapa)) && not(any (\hitboxbloco -> colisaoHitbox (calculaHitboxEmCima personagem) hitboxbloco) (hitboxesBlocos(mapaTrampolins mapa))) && (snd $ velocidade personagem)>0
    then personagem {velocidade = (fst $ (velocidade personagem), -2)} 
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

-- | Função que aplica o efeito de bounce 
bounce :: Mapa -> Tempo -> Personagem -> Personagem
bounce mapa tempo jogador =
  if any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa)) && not(any (\hitbox -> colisaoHitbox (calculaHitboxDireita jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa))) && not(any (\hitbox -> colisaoHitbox (calculaHitboxEsquerda jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa)))
    then jogador {velocidade = (fst $ velocidade jogador, (-8) + (snd gravidade) * tempo)}
  else jogador

-- | Função que atualiza o booleano da impulsao
atualizaImpulsao :: Mapa -> Personagem -> Personagem
atualizaImpulsao mapa jogador = 
  if (any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa)) && not(any (\hitbox -> colisaoHitbox (calculaHitboxDireita jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa))) && not(any (\hitbox -> colisaoHitbox (calculaHitboxEsquerda jogador) hitbox) (hitboxesBlocos(mapaTrampolins mapa)))) || any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitbox) (hitboxesBlocos(mapaLancas mapa)) && not(any (\hitbox -> colisaoHitbox (calculaHitboxDireita jogador) hitbox) (hitboxesBlocos(mapaLancas mapa))) && not(any (\hitbox -> colisaoHitbox (calculaHitboxEsquerda jogador) hitbox) (hitboxesBlocos(mapaLancas mapa))) && not(any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitbox) (hitboxesBlocos(mapaPlataformas mapa)))
    then jogador{impulsao=True}
  else if any (\hitbox -> colisaoHitbox (calculaHitboxEmbaixo jogador) hitbox) (hitboxesBlocos(mapaPlataformasAlcapoes blocos))
    then jogador{impulsao=False}
  else jogador
  where (Mapa _ _ blocos) = mapa