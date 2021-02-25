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
Render, reders the game\\
by reading the state\\
display:
- the map
- score
- (menu? restard button when player dies?)
-}

main :: IO ()
main = do {
    display window background drawing
}