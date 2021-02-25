import Graphics.Gloss


-- Initialize an empty world
-- world is an array of cells
-- parameter is the number of cells
-- a cell is a character that's either
-- E: empty, W: wall, L: light
makeWorld::Int -> String -> String
makeWorld 0 s = s;
makeWorld x s = makeWorld (x-1) ('E':s)


-- nextWorld
{- 
make the next frame of the world
reads the old world to see what is permited to add as the next cell
(random number generator?)
-}
-- nextWorld:: String -> String

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


-- temp place holder
drawing :: Picture
drawing = circle 80


{-
Render
takes the state and make the viasuals
-}
-- render:: _____ -> Picture


{-
handleKeys
takes the keystroke, the state, and updates the state
-}
-- handleKeys Event -> GameState -> GameState



-- update
{-
The function that changes the world after each iteration
-}


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

    -- play window background 30 state render handleKeys update