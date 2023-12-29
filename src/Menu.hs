module Menu where
import LI12324
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import GHC.Float 
import GHC.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Estadomenu = Menu  
                  Opcao
                  Bool

data Opcao = Jogar
            |Settings
            |Sair
            deriving(Eq, Read, Show)


reagemenu :: Event -> Estadomenu -> Estadomenu
reagemenu (EventKey (SpecialKey KeyDown) Down _ _ ) (Menu o b)
            |o == Jogar = (Menu Settings False)
            |o == Settings = (Menu Sair False)
            |o == Sair = (Menu Jogar False)

reagemenu (EventKey (SpecialKey KeyUp) Down _ _ ) (Menu o b)
            |o == Jogar = (Menu Sair False)
            |o == Settings = (Menu Jogar False)
            |o == Sair = (Menu Settings False)


reagemenu (EventKey (SpecialKey KeyEnter)) (Menu o b)
            |o == Sair = undefined
            |otherwise = (Menu o True)


