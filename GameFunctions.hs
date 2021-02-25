module GameFunctions where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Point
import System.Random

-- makes a world of e cells
-- a cell is a character that's either
-- E: empty, W: wall, L: light
makeWorld::Int -> String -> String
makeWorld 0 s = s;
makeWorld x s = makeWorld (x-1) ('E':s)


-- Structs
{-
GameState is a struct of world, ghost, score, alive
ghost is whether the ghost is invisible or not (Invisible = True, Visible = False)
score is the number of walls passes
alive is whether not not the player is alive
-}
data GameState = GameState {
    world::String,
    ghost::Bool,
    score::Int,
    alive::Bool,
    fade::Int
}


-- pics is just so images can be accessed easier in render
data Pics = Pics{
    land::Picture,
    wall::Picture,
    lamp::Picture,
    ghost_norm::Picture,
    ghost_lit::Picture,
    ghost_invis::Picture
}

window :: Display
window = InWindow "FlappyGhost" (1000, 504) (50, 50)

background :: Color
background = black

gameOver:: String -> Picture
gameOver score = Pictures
                    [(translate 450 200 $ scale 0.3 0.3 $ color white $ text score),
                    translate (-300) 0 $ color white $ text "GameOver"]


-- Render
-- takes the state and make the visuals
-- 40 (25 pix width) cells on screen at a time
render:: Pics -> GameState -> Picture
render pics state
    | (alive state) && (ghost state) = 
        renderGame pics state (ghost_invis pics)
    | (alive state) =
        renderGame pics state (ghost_norm pics)
    | otherwise = gameOver $ show (score state)     

renderGame:: Pics -> GameState -> Picture -> Picture
renderGame pics state ghost = 
    Pictures (renderHelper 
                pics 
                ((translate 450 200 $ scale 0.3 0.3 $ color white $ text $ show (score state)):
                    [ghost])
                (world state) 
                (-1))

renderHelper:: Pics -> [Picture] -> String -> Float -> [Picture]
renderHelper pics ps [] x = ps
renderHelper pics ps (h:t) x 
    | x == -1 = (land pics):(renderHelper pics ps t (x+1))
    | h == 'W' = 
        (translate (-400 + (x * 25)) 0 (wall pics)):(renderHelper pics ps t (x+1))
    | h == 'L' =
         (translate (-400 + (x * 25)) 0 (lamp pics)):(renderHelper pics ps t (x+1))
    |otherwise = renderHelper pics ps t (x+1)


{-
handleKeys
takes the keystroke, the state, and updates the state
-}
handleKeys:: Event -> GameState -> GameState
handleKeys (EventKey k ks _ _) gs
    | SpecialKey KeySpace <- k = gs{ ghost = True, fade = 5}
    | otherwise = gs
handleKeys _ gs = gs



-- update
{-
The function that changes the world after each iteration
-}
update::float -> GameState -> GameState
update f state = 
    if 
        (alive state)
    then 
        GameState {
        world = uHelper (world state) [] 0,
        ghost = if (fade state == 0) then False else True,
        score = checkScore (world state) 0 (score state),
        alive = checkAlive (world state) (ghost state) 0,
        fade = if (fade state > 0) then (fade state) - 1 else 0
        }
    else
        state

uHelper [] ns x = "E"
uHelper (h:t) ns x
    |x == 0 = uHelper t ns (x+1)
    |otherwise = h:(uHelper t ns x)

checkAlive::String -> Bool -> Int -> Bool
checkAlive [] ghost count = True
checkAlive (h:t) ghost count
    | count < 11 = True && (checkAlive t ghost (count+1))
    | count > 16 = True
    | h == 'W' && ghost = True && (checkAlive t ghost (count+1))
    | h == 'W' = False
    | count > 12 && count < 15 && h == 'L' && ghost = False
    | otherwise = True && (checkAlive t ghost (count+1))

checkScore::String -> Int -> Int -> Int
checkScore [] count score = score
checkScore (h:t) count score
    |count < 13 = checkScore t (count+1) score
    |count == 13 && h == 'W' = score + 1
    |otherwise = score
