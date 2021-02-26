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
glit is whether the ghost is in light mode or not (light mode = True, Normal = False)
score is the number of walls passes
alive is whether not not the player is alive
-}
data GameState = GameState {
    world::String,
    ghost::Bool,
    glit::Bool,
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
    | (alive state) && (ghost state) && not(glit state) = 
        renderGame pics state (ghost_invis pics)
    | (alive state) && (glit state) && not(ghost state) =
        renderGame pics state (ghost_lit pics)
    | (alive state) =
        renderGame pics state (ghost_norm pics)
    | otherwise = gameOver $ show (score state)     

renderGame:: Pics -> GameState -> Picture -> Picture
renderGame pics state ghost = 
    Pictures (renderHelper 
                pics 
                ((translate 400 200 $ scale 0.3 0.3 $ color white $ text $ show (score state)):
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
    | SpecialKey KeyDown <- k = gs{ ghost = True, glit = False, fade = 3}
    | SpecialKey KeyUp <- k = gs{ ghost = False, glit = True, fade = 3}
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
        world = updateWorld (world state) 0 state,
        ghost = if (fade state == 0) then False else True,
        glit = if (fade state == 0) then False else True,
        score = updateScore (world state) 0 (score state),
        alive = updateAlive (world state) (ghost state) (glit state) 0,
        fade = if (fade state > 0) then (fade state) - 1 else 0
        }
    else
        state


updateAlive::String -> Bool -> Bool -> Int -> Bool
updateAlive [] ghost glit count = True
updateAlive (h:t) ghost glit count
    | count > 7 && count < 15 && h == 'L' && glit = True
    | count < 8 = True && (updateAlive t ghost glit (count+1))
    | count > 14 = True
    | h == 'W' && ghost = True && (updateAlive t ghost glit (count+1))
    | h == 'W' = False
    | h == 'L' && glit = True && (updateAlive t ghost glit (count+1))
    | h == 'L' = False
    | otherwise = True && (updateAlive t ghost glit (count+1))



updateScore::String -> Int -> Int -> Int
updateScore [] count score = score
updateScore (h:t) count score
    |count < 13 = updateScore t (count+1) score
    |count == 13 && (h == 'W'|| h == 'L') = score + 1
    |otherwise = score


updateWorld::String -> Int -> GameState -> String
updateWorld [] x state
    |x > 50 = []
    |otherwise = generateWorld 30 18 27 5 11 state

updateWorld (h:t) count state
    |count == 0 = updateWorld t (count+1) state
    |otherwise = h:(updateWorld t (count+1)) state

{- rando selects an integer from x to y, based on the score of the state.-}
rando::Int -> Int -> GameState -> Int
rando x y state = [x..y]!!(randoIndex x y state)

{- randoIndex gives an index of the list [x..y] determined by the score of the state.-}
randoIndex::Int -> Int -> GameState -> Int
randoIndex x y state = mod (score state) (listLength x y)

{- listLength makes a list from x to y and returns the length.-}
listLength::Int -> Int -> Int
listLength x y = length[x..y]

-- total: the length
-- x1: lower bound of location of wall
-- x2: upper bound of location of wall
-- y3: lower bound of location of lamp
-- y4: upper bound of loaction of lamp
generateWorld::Int -> Int -> Int -> Int -> Int -> GameState -> String
generateWorld total x1 x2 y1 y2 state
    |total == 0 = "E"
    |(total == x) = 
        'W':(generateWorld (total-1) x1 x2 y1 y2 state)  
    |(total == y) =
        'L':(generateWorld (total-1) x1 x2 y1 y2 state)
    |otherwise = 'E':(generateWorld (total-1) x1 x2 y1 y2 state)
    where
        x = rando x1 x2 state
        y = rando y1 y2 state


