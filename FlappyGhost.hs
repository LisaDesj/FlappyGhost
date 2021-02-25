import Graphics.Gloss


-- Initialize an empty world
-- world is an array of cells
-- parameter is the number of cells
-- a cell is a character that's either
-- E: empty, W: wall, L: light
makeWorld::Int -> String -> String
makeWorld 0 s = s;
makeWorld x s = makeWorld (x-1) ('E':s)

{-
GameState is a struct of world, ghost, score, alive
ghost is whether the ghost is invisible or not
score is the number of walls passes
alive is whether not not the palyer is alive
-}
data GameState = GameState {
    world::String,
    ghost::Bool,
    score::Int,
    alive::Bool
}

window :: Display
window = InWindow "FlappyGhost" (800, 400) (50, 50)

background :: Color
background = black

{-
Render
takes the state and make the viasuals
-}
-- render:: _____ -> Picture
-- temp render
-- render::GameState -> Picture
-- render = 


{-
handleKeys
takes the keystroke, the state, and updates the state
-}
-- handleKeys Event -> GameState -> GameState



-- update
{-
The function that changes the world after each iteration
-}

-- =======================
-- temp place holders
-- to mock the game and make sure things work
drawing :: Picture
drawing = Pictures [translate (-100) (-100) $ color blue $ rectangleSolid 80 200, 
                    translate (100) (-100) $ color white $ rectangleSolid 80 200]

tempRender state = Pictures (trHelper (world state) [] 0)

trHelper:: String -> [Picture] -> Float -> [Picture]
trHelper [] [] x = []
trHelper [] ps x = ps
trHelper (h:t) ps x
    | h == 'E' = 
        trHelper t ((translate (-400 + (x * 100)) (-100) $ color blue $ rectangleSolid 80 200):ps) (x+1)
    | otherwise = 
        trHelper t ((translate (-400 + (x * 100)) (-100) $ color white $ rectangleSolid 80 200):ps) (x+1)

tempHK event state = state

tempUpdate f state = GameState {
    world = tuHelper (world state) [] 0,
    ghost = True,
    score = 0,
    alive = True
}
    

tuHelper [] ns x = "E"
tuHelper (h:t) ns x
    |x == 0 = tuHelper t ns (x+1)
    |otherwise = h:(tuHelper t ns x)

tempState = GameState {
    world = "EEEEEEEEEW",
    ghost = True,
    score = 0,
    alive = True
}
-- ========================



{-
main is where all the functions are put together and used via gloss
-}
main :: IO ()
main = do
    -- load all the bmp images here
    let wd = makeWorld 10 ""
    let state = GameState {
        world = wd,
        ghost = True,
        score = 0,
        alive = True
   }

    -- display window background drawing
    -- play window background 30 state render handleKeys update

    -- moack
    play window background 10 tempState tempRender tempHK tempUpdate