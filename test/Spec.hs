module Main where
import LI12324
import Mapas
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Utilities
import Test.HUnit


testColisoesParede :: Test
testColisoesParede = test
    [ "Teste com personagem dentro dos limites sem estar em plataforma" ~: do
        let mapa = mapa1
        let personagem = Personagem {posicao = (2,3), tamanho= (1,1)}
        assertEqual "Dentro dos limites" False (colisoesParede mapa personagem)
    , "Teste com personagem fora dos limites laterais" ~: do
        let mapa = mapa1
        let personagem = Personagem {posicao = (30,3), tamanho=(1,1)}
        assertEqual "Fora dos limites laterais" True (colisoesParede mapa personagem)
    , "Teste com personagem acima do limite superior" ~: do
        let mapa = mapa1
        let personagem = Personagem {posicao = (2,-5), tamanho=(1,1)}
        assertEqual "Acima do limite superior" True (colisoesParede mapa personagem)
    , "Teste dentro dos limites mas a tocar numa plataforma" ~: do 
        let mapa = mapa1
        let personagem = Personagem {posicao = (2,19), tamanho=(1,1)}
        assertEqual "A tocar numa Plataforma" True (colisoesParede mapa personagem)
    ]

testColisoesPersonagem :: Test
testColisoesPersonagem = test 
    ["Teste para testar colisão entre dois personagens" ~: do 
        let personagem1 = Personagem {posicao = (2,3), tamanho= (1,1)}
        let personagem2 = Personagem {posicao = (3,3), tamanho= (1,1)}
        assertEqual "As duas personagens a colidir" True (colisoesPersonagens personagem1 personagem2)
    ]


testvalida :: Test
testvalida = test
    [
        "Teste para confirmar se o jogo foi validado" ~: do
            let personagem1 = Personagem {tamanho = (1,1), ressalta = False, tipo = Jogador}
            let colecionaveis1 = [(Martelo,(3,2))]
            let listaInimigos = [(Personagem {posicao = (4,5), tamanho = (0.2,0.2), ressalta = True,tipo = Fantasma, vida = 1})]
            let jogo1 = Jogo {mapa = mapa1, jogador = personagem1, colecionaveis = colecionaveis1, inimigos = listaInimigos}
            assertEqual "O jogo a validar" True (valida jogo1)







    ]


test_suite_01 = test ["Basic Test" ~: True ~=? True]

main :: IO ()
main = runTestTTAndExit $ test [test_suite_01,testColisoesParede,testColisoesPersonagem,testvalida]
