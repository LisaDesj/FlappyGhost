import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


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


data Pics = Pics{
    land::Picture,
    wall::Picture,
    lamp::Picture,
    ghost_norm::Picture,
    ghost_lit::Picture,
    ghost_invis::Picture
}

gameOver::Picture
gameOver = translate (-300) 0 $ color white $ text "GameOver"

{-
Render
takes the state and make the viasuals
-}
render:: Pics -> GameState -> Picture
render pics state
    | (alive state) && (ghost state) = 
        Pictures (renderHelper pics [(translate (-350) (-100) (ghost_invis pics))] (world state) 0)
    | (alive state) =
        Pictures (renderHelper pics [(translate (-350) (-100) (ghost_norm pics))] (world state) 0)
    | otherwise = gameOver

        

renderHelper:: Pics -> [Picture] -> String -> Float -> [Picture]
renderHelper pics ps [] x = ps
renderHelper pics ps (h:t) x
    | h == 'W' = 
        (translate (-400 + (x * 50)) (-100) (wall pics)):(renderHelper pics ps t (x+1))
    | h == 'L' =
         (translate (-400 + (x * 50)) (-100) (lamp pics)):(renderHelper pics ps t (x+1))
    |otherwise = renderHelper pics ps t (x+1)


{-
handleKeys
takes the keystroke, the state, and updates the state
-}
-- handleKeys Event -> GameState -> GameState
handleKeys (EventKey k _ _ _) gs
    | SpecialKey KeyDown <- k   = gs{ ghost = False }
    | otherwise = gs
--TO DO: Add timer



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
    land <- loadBMP "images/land01.bmp"
    wall <- loadBMP "images/wall01.bmp"
    light <- loadBMP "images/light01.bmp"
    ghost_norm <- loadBMP "images/ghost.bmp"
    ghost_lit <- loadBMP "images/ghost_light.bmp"
    ghost_invis <- loadBMP "images/ghost_invis.bmp"
    sample <- loadBMP "images/sample.bmp"
    trial <- loadBMP "images/trial.bmp"
    let wd = makeWorld 10 ""

    let state = GameState {
        world = wd,
        ghost = True,
        score = 0,
        alive = True
    }
    let pics = Pics {
        land = scale 800 80 land,
        wall = scale 50 300 wall,
        lamp = scale 50 80 light,
        ghost_norm = scale 40 40 ghost_norm,
        ghost_lit = scale 40 40 ghost_lit,
        ghost_invis = scale 40 40 ghost_invis
    }
    -- play window background 30 state render handleKeys update

    -- mock
    -- play window background 10 tempState tempRender tempHK tempUpdate

    -- display window background (scale 0.2 0.2 sample)
    display window white (scale 2 2 trial)
    -- play window background 50 tempState (render pics) tempHK tempUpdate