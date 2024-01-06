module DrawMenu where


import LI12324
import Utilities
import Niveis
import System.Exit
import Data.Char
import Data.List
import Data.Maybe
import GHC.Float 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


drawMenu :: State -> Picture
drawMenu state = Pictures [selectButton (currentMenu state) (images state) (selectedButton state),drawHighScore state]

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
  | selectedButton state == 2 = state {currentLevel=currentLevel initialState,levelsList= (levelsList initialState),currentMenu = Home,selectedButton=0} --Home
  | selectedButton state == 1 = state {currentLevel=currentLevel initialState,levelsList= (levelsList initialState),currentMenu = InGame,selectedButton=0} --Retry
reactPause e state = state

reactGameOver :: Event -> State -> State
reactGameOver (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactGameOver (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 1 then selectedButton state + 1 else selectedButton state} 
reactGameOver (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentLevel=currentLevel initialState,levelsList=(levelsList initialState),currentMenu = InGame,selectedButton=0} --Resume
  | selectedButton state == 1 = state {currentLevel=currentLevel initialState,levelsList=(levelsList initialState),currentMenu = Home,selectedButton=0} --Retry
reactGameOver e state = state

reactOptions :: Event -> State -> State
reactOptions (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactOptions (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 2 then selectedButton state + 1 else selectedButton state} 
reactOptions (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = Mode,selectedButton=0} --Mode
  | selectedButton state == 1 = state {currentMenu = Themes,selectedButton=0} --Themes
  | selectedButton state == 2 = state {currentMenu = Home,selectedButton=0} --Back
reactOptions e state = state

reactMode :: Event -> State -> State
reactMode (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactMode (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 2 then selectedButton state + 1 else selectedButton state} 
reactMode (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = Options,currentMode = Easy,selectedButton=0} --Easy
  | selectedButton state == 1 = state {currentMenu = Options,currentMode = Medium,selectedButton=0} --Medium
  | selectedButton state == 2 = state {currentMenu = Options,currentMode = Hard,selectedButton=0} --Hard
reactMode e state = state

reactThemes :: Event -> State -> State
reactThemes (EventKey (SpecialKey KeyUp) Down _ _) state = state {selectedButton = if selectedButton state > 0 then selectedButton state - 1 else selectedButton state} 
reactThemes (EventKey (SpecialKey KeyDown) Down _ _) state = state {selectedButton = if selectedButton state < 4 then selectedButton state + 1 else selectedButton state} 
reactThemes (EventKey (SpecialKey KeyEnter) Down _ _) state 
  | selectedButton state == 0 = state {currentMenu = Options,currentTheme=Mario,selectedButton=0} --Mario
  | selectedButton state == 1 = state {currentMenu = Options,currentTheme=MarioCat,selectedButton=0} --MarioCat
  | selectedButton state == 2 = state {currentMenu = Options,currentTheme=MarioBear,selectedButton=0} --MarioBear
  | selectedButton state == 3 = state {currentMenu = Options,currentTheme=MarioFrog,selectedButton=0} --MarioFrog
  | selectedButton state == 4 = state {currentMenu = Options,currentTheme=MarioAstronaut,selectedButton=0} --MarioAstronaut
reactThemes e state = state

drawHighScore :: State -> Picture
drawHighScore state = 
  if currentMenu state == Home 
    then Pictures (drawPoints (highScore state))
  else blank
  where
    drawPoints :: Int -> [Picture]
    drawPoints p = foldl (\pic n -> [Translate ((fromIntegral(length pic)*50-(27*fromIntegral(length ps-1)))) (-500) $ scale (0.8) (0.8) $ (fromJust(lookup ([n]) imagesThemeDef))] ++ pic) [] ps
      where ps = show p
    imagesThemeDef = fromJust (lookup Mario (images state))
