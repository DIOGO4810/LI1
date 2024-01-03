module DrawMenu where


import LI12324
import Utilities
import Niveis
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import GHC.Float 
import GHC.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


drawMenu :: State -> Picture
drawMenu state = selectButton (currentMenu state) (images state) (selectedButton state)

selectButton :: Menu -> Images-> Int -> Picture
selectButton menu images n = fromJust(lookup ("botao" ++ show (n+1) ++ show menu) imagesThemeDef)
  where imagesThemeDef = fromJust (lookup Mario images)

reactMenu :: Event -> State -> State
reactMenu (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactMenu (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 2 then selectedButton state + 1 else selectedButton state} 
reactMenu (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = InGame,selectedButton=0} --Play
  | selectedButton state == 1 = state {currentMenu = Options,selectedButton=0} --Settings
  | selectedButton state == 2 = state {exitGame = True} --Exit
reactMenu e state = state

reactPause :: Event -> State -> State
reactPause (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactPause (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 2 then selectedButton state + 1 else selectedButton state} 
reactPause (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = InGame,selectedButton=0} --Resume
  | selectedButton state == 2 = state {levelsList=updateLevel (levelsList state) (currentLevel state, (levelsList initialState)!!currentLevel state),currentMenu = Home,selectedButton=0} --Home
  | selectedButton state == 1 = state {levelsList=updateLevel (levelsList state) (currentLevel state, (levelsList initialState)!!currentLevel state),currentMenu = InGame,selectedButton=0} --Retry
reactPause e state = state

reactGameOver :: Event -> State -> State
reactGameOver (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactGameOver (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 1 then selectedButton state + 1 else selectedButton state} 
reactGameOver (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {levelsList=updateLevel (levelsList state) (currentLevel state, (levelsList initialState)!!currentLevel state),currentMenu = InGame,selectedButton=0} --Resume
  | selectedButton state == 1 = state {levelsList=updateLevel (levelsList state) (currentLevel state, (levelsList initialState)!!currentLevel state),currentMenu = Home,selectedButton=0} --Retry
reactGameOver e state = state

reactOptions :: Event -> State -> State
reactOptions (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactOptions (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 4 then selectedButton state + 1 else selectedButton state} 
reactOptions (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = Home,currentTheme=Mario,selectedButton=0} --Mario
  | selectedButton state == 1 = state {currentMenu = Home,currentTheme=MarioCat,selectedButton=0} --MarioCat
  | selectedButton state == 2 = state {currentMenu = Home,currentTheme=MarioBear,selectedButton=0} --MarioBear
  | selectedButton state == 3 = state {currentMenu = Home,currentTheme=MarioFrog,selectedButton=0} --MarioFrog
  | selectedButton state == 4 = state {currentMenu = Home,currentTheme=MarioAstronaut,selectedButton=0} --MarioAstronaut
reactOptions e state = state