import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.Point
import System.Random
import GameFunctions

-- =======================
-- temp place holders
-- to mock the game and make sure things work
drawing :: Picture
drawing = Pictures [translate (-100) (-100) $ color blue $ rectangleSolid 80 200, 
                    translate (100) (-100) $ color white $ rectangleSolid 80 200]

tempState = GameState {
    world = "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEWEEEEEEEEEEEEEEEEEEEEEEEELEEEEEEEEEEEEEWEEEEEEEEEEEE",
    ghost = False,
    glit = False,
    score = 0,
    alive = True,
    fade = 0
}
-- ========================



{-
main is where all the functions are put together and used via gloss
-}
main :: IO ()
main = do
    -- load all the bmp images here
    land <- loadBMP "images/land.bmp"
    wall <- loadBMP "images/wall.bmp"
    light <- loadBMP "images/light.bmp"
    ghost_norm <- loadBMP "images/ghost.bmp"
    ghost_lit <- loadBMP "images/ghost_light.bmp"
    ghost_invis <- loadBMP "images/ghost_invis.bmp"
    -- sample <- loadBMP "images/sample.bmp"
    -- trial <- loadBMP "images/trial.bmp"
    let wd = makeWorld 10 ""

    let state = GameState {
        world = wd,
        ghost = False,
        glit  = False,
        score = 0,
        alive = True,
        fade = 0
    } 
    let pics = Pics {
        land = scale 1 1.5 land,
        -- wall width = 100, lamp width = 200, ghost width = 50
        -- wall 4 cells, lamp = 8 cells, ghost 2 cells
        wall = scale 0.91 1.5 wall,
        lamp = scale 1.14 1.5 light,
        ghost_norm = translate (-175) 0 $ scale 0.714 0.714 ghost_norm,
        ghost_lit = translate (-175) 0 $ scale 0.714 0.714 ghost_lit,
        ghost_invis = translate (-175) 0 $ scale 0.714 0.714 ghost_invis
    }
    -- play window background 30 state render handleKeys update

    -- mock
    -- play window background 10 tempState tempRender tempHK tempUpdate

    -- display window background (scale 0.2 0.2 sample)
    -- display window white (scale 0.2 0.2 trial)
    -- display window background (translate 450 200 $ scale 0.3 0.3 $ color white $ text $ show (score state))
    play window background 8 tempState (render pics) handleKeys update