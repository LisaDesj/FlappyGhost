module FlappyGhost where


-- State
 {- 
 State is a struct consist of:
 - score (an integer, # number of walls passed)
 - alive (a boolean)
 - map
 -}




-- Ghost
{-
Ghost is a struct of:
- mode (a bool, either invisble or not)
-}



-- map
{-
the map is 10 (or some other number) vertical cells
the ghost is in the first cell
a cell is either blank, wall, or light

(Note*
have some empepty cells between wall and light,
to give player time to press)

(use random number generator?
to decide number of cells between walls, lights)
-}



-- Game [Action] -> ghost -> state -> state
{-
game takes in an action, which updates ghost
and update state based on ghost's mode and the map
-}