module Main where
import LI12324
import Niveis
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Utilities
import Test.HUnit
import Data.List



mapatestes = Mapa ((1.5, 22.5), Este) (7.5, 2.0) (abreviaBlocos 
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
    ,['V','P','P','P','P','P','A','A','V','P','P','P','P','P','V']
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





testColisoesParede :: Test
testColisoesParede = test
    [ "Teste com personagem dentro dos limites sem estar em plataforma" ~: do

        let mapa = mapa1
        let personagem = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "Dentro dos limites" False (colisoesParede mapa personagem)

    , "Teste com personagem fora dos limites laterais" ~: do

        let mapa = mapa1
        let personagem = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(30,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "Fora dos limites laterais" True (colisoesParede mapa personagem)

    , "Teste com personagem acima do limite superior" ~: do

        let mapa = mapa1
        let personagem = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,-5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "Acima do limite superior" True (colisoesParede mapa personagem)

    , "Teste dentro dos limites mas a tocar numa plataforma" ~: do

        let mapa = mapa1
        let personagem = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,19),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "A tocar numa Plataforma" True (colisoesParede mapa personagem)
    ]

testColisoesPersonagem :: Test
testColisoesPersonagem = test
    ["Teste para testar colisão entre dois personagens" ~: do

        let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
        let personagem2 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(3,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "As duas personagens a colidir" True (colisoesPersonagens personagem1 personagem2)

    ,"Teste para testar se ao dois personagens não estarem a colidir a função devolde False " ~: do

        let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
        let personagem2 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(6,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}

        assertEqual "As duas personagens a não colidir" False (colisoesPersonagens personagem1 personagem2)

    ]


testvalida :: Test
testvalida = test
    [
        "Teste para confirmar se o jogo foi validado" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let colecionaveis1 = [(Martelo,(3,2))]
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (10.5,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 1,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (1.5,2.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 1,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo a validar" True (valida jogo1)

        ,"Teste para confirmar se o jogo não é validado quando só tem um fantasma" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(2,3),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let colecionaveis1 = [(Martelo,(3,2))]
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (10.5,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 1,aplicaDano = (False,0), pontos = 0})]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo não a validar" False (valida jogo1)

        ,"Teste para confirmar se o jogo não é validado quando um jogador nasce no mesmo sitio que outro personagem" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(1.5,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let colecionaveis1 = [(Martelo,(3,2))]
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (1.5,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 1,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (1.5,2.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 1,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo a não validar" False (valida jogo1)

        ,"Teste para confirmar se o jogo não é validado quando um personagem ou um colecionável nasce dentro de um bloco" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(5,23),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let colecionaveis1 = [(Martelo,(2,23))]
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (10.5,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 1,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (1.5,2.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 1,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo a não validar" False (valida jogo1)
    ]

testemovimenta :: Test
testemovimenta = test
    [
        "Teste para confirmar se a funçao faz com que o inimigo perca um de vida em contacto com o jogador" ~: do
            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (1,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 5,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (2,22.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 5,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let colecionaveis1 = [(Martelo,(3,2))]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo a movimentar corretamente" True ((vida . head . inimigos . movimenta 100 1 $ jogo1) < 5)

        ,"Teste para confirmar se a funçao faz com que o jogador perca um de vida em contacto com o inimigo" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (True,10), pontos = 0}
            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (1,22.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 5,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (2,22.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 5,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let colecionaveis1 = [(Martelo,(3,2))]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos1}

            assertEqual "O jogo a movimentar corretamente" True  ((vida . jogador. movimenta 100 1 $ jogo1) < 50)

        ,"Teste para confirmar se a funçao faz com que o jogador a obter o martelo ganha as habilidades do mesmo" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (4,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let colecionaveis1 = [(Martelo,(1,22.5))]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = []}

            assertEqual "O jogo a movimentar corretamente" True  ((fst . aplicaDano . jogador. movimenta 100 1 $ jogo1))

        ,"Teste para confirmar se a funçao faz com que o jogador não caia quando está na escada" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = True, velocidade = (0,0),posicao =(2.5,8),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let colecionaveis1 = [(Martelo,(9,22.5))]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = []}

            assertEqual "O jogo a movimentar corretamente" jogo1  (movimenta 100 1 jogo1)

        ,"Teste para confirmar se a funçao impede que o jogador saia fora dos limites do mapa" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = True, velocidade = (-1,0),posicao =(0.5,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

            assertEqual "O jogo a movimentar corretamente" True  ((fst . posicao . jogador. movimenta 100 1 $ jogo1) >= 0)

        ,"Teste para confirmar se a funçao faz com que os fantasmas não consigam ativar as plataformas mas o jogador sim" ~: do

            let listaInimigos1 = [(Personagem {direcao = Oeste, escudo = (False , 0),impulsao = False,kickback = (False,0) , emEscada = False, posicao = (7,10.5), tamanho = (1,1), ressalta = True,tipo = Fantasma, velocidade = (0,0),vida = 5,aplicaDano = (False,0), pontos = 0}),(Personagem {posicao = (2,22.5),impulsao = False,kickback = (False,0),escudo = (False , 0) , tamanho = (1,1), ressalta = True,tipo = Fantasma, vida = 5,aplicaDano = (False,0), pontos = 0,velocidade = (0,0),direcao = Este , emEscada = False})]
            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (0,0),posicao =(9,10.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = listaInimigos1}

            assertEqual "O jogo a movimentar corretamente" mapatestes  (mapa . movimenta 100 1 $ jogo1)

        ,"Teste para confirmar se a funçao faz com que o jogador ganhe pontos a recolher uma moeda" ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (0,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let colecionaveis1 = [(Moeda,(1,22.5))]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = []}

            assertEqual "O jogo a movimentar corretamente" True  ((pontos . jogador . movimenta 100 1 $ jogo1) > 0)

         ,"Teste para confirmar se a funçao faz com que o jogador seja afetado pela gravidade " ~: do

            let personagem1 = Personagem{direcao = Este,impulsao = False,kickback = (False,0), escudo = (False , 0) , emEscada = False, velocidade = (0,0),posicao =(2,20.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

            assertEqual "O jogo a movimentar corretamente" True  ((snd . velocidade . jogador . movimenta 100 1 $ jogo1) > 0)

    ]





testAtualiza :: Test
testAtualiza = test
 [
    "Teste para confirmar se o jogador ganhou velocidade para a direita a receber a ação AndarDireita" ~: do

        let acoesjogador = Just AndarDireita
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = False, velocidade = (0,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona corretamente" True ((fst . velocidade . jogador . atualiza [] acoesjogador $ jogo1) > 0)

    ,"Teste para confirmar se o jogador perde toda a sua velocidade a receber a ação Parar" ~: do

        let acoesjogador = Just Parar
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = False, velocidade = (0,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona corretamente" True ((velocidade . jogador . atualiza [] acoesjogador $ jogo1) ==(0,0))

    ,"Teste para confirmar se o jogador muda de direção" ~: do

        let acoesjogador = Just AndarEsquerda
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = False, velocidade = (0,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona corretamente" True ((direcao . jogador . atualiza [] acoesjogador $ jogo1) == Oeste)

    ,"Teste para confirmar se o jogador ganha velocidade em Y negativa a saltar" ~: do

        let acoesjogador = Just Saltar
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = False, velocidade = (0,0),posicao =(1,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona corretamente" True ((snd . velocidade . jogador . atualiza [] acoesjogador $ jogo1) < 0)

    ,"Teste para confirmar se o jogador ganha velocidade em Y negativa ao subir uma escada" ~: do

        let acoesjogador = Just Subir
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = True, velocidade = (0,0),posicao =(7.5,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona correatemente" True ((snd . velocidade . jogador . atualiza [] acoesjogador $ jogo1) < 0)

    ,"Teste para confirmar se o jogador ganha velocidade em Y positiva ao descer uma escada" ~: do

        let acoesjogador = Just Descer
        let personagem1 = Personagem {direcao = Este , impulsao = False,kickback = (False,0),escudo = (False,0), emEscada = True, velocidade = (0,0),posicao =(7.5,22.5),tamanho = (1,1), ressalta = False, tipo = Jogador, vida = 50, aplicaDano = (False,0), pontos = 0}
        let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = [], inimigos = []}

        assertEqual "Verificar se a função atualiza funciona corretamente" True ((snd . velocidade . jogador . atualiza [] acoesjogador $ jogo1) > 0)

 ]



test_suite_01 :: Test
test_suite_01 = test ["Basic Test" ~: True ~=? True]

main :: IO ()
main = runTestTTAndExit $ test [test_suite_01,testColisoesParede,testColisoesPersonagem,testvalida,testAtualiza,testemovimenta]

