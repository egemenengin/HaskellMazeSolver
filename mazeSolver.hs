{- 
Egemen Engin
-}

{-
This function find starting point. It starts from 0 0 and it searches recursively until it finds.
If it cant find, it returns empty list in the end.
-}
startFinder ::   [[Char]] ->Int ->Int -> [Int]
startFinder  mazeArr x y 
    |y >= length(mazeArr) = []   
    |(x == (length(mazeArr !! y))) = startFinder mazeArr 0 (y + 1)
    |((mazeArr !! y) !!x) == 'S' = [x,y]
    |(x < length((mazeArr !! y)))= startFinder mazeArr (x + 1) y

{-
This function call another function which is mazeSolver if input 2d list is not empty and returns steps of path from start to exit.
If it is empty it returns empty list.
Calling this function like "solveMaze maze" is enough.
-}
solveMaze :: [[Char]] -> [Char]
solveMaze mazeList
    |mazeList /= [] = mazeSolver mazeList ((startFinder mazeList 0 0)!! 0) ((startFinder mazeList 0 0)!! 1) []  []((startFinder mazeList 0 0)!! 0) ((startFinder mazeList 0 0)!! 1) []
    | mazeList == [] = [] 
    
{-
This function finds stepst of path from start to ends recursively and it returns it.
Firstly, it checks current coordinates is in the maze or not. If it is not in maze it starts from starting point again.
Secondly, it checks starting point is in blackOutCoordineates or not which means there is no way to go finish point.
After that, it checks the point is finish point or not.
If these are not, it tries to go up and add "U" to string which includes steps of path. 
If there is a block, it tries to go down and "D" to string which includes steps of path. 
It goes on like these for right and left.
If there is no way to go this point is added to blackedOutCoordinates, it clears the string which includes steps of path and it starts from starting point.

As a result, if it has nowhere to go or if goes until it gets stuck, and if that point isn't the exit point, it adds it to the blackedOutList and starts over.
In the end, if there is a way from the beginning to the exit, the remaining points are the solution . 
Of course, in the best-case scenario, it can find the solution directly, 
or in the worst-case scenario, it may need to add all the points to the list except the result path points to find the result path.

Except boundary check, there is important checks which are :
Program sees this blackedOutCoordinates list as an obstacle.
Also it checks every point is it block or not too before it goes there.
It doesn't again pass the point which is included in current path coordinates.

-}
mazeSolver :: [[Char]] -> Int -> Int -> [[Int]]-> [[Int]] -> Int -> Int ->[Char] -> [Char]
mazeSolver maze curX curY blackOutCoordinates currentPath startX startY foundPath
    | curY >= length(maze) || curX >= (length (maze !! curY )) = mazeSolver maze startX startY ([[curX,curY]] ++ blackOutCoordinates ) [] startX startY []
    | elem [startX, startY] blackOutCoordinates = []
    |(maze !! curY)!! curX == 'E'= foundPath
    | curY - 1 >= 0 && ((maze !! (curY-1))!! curX)/='#' && notElem [curX,curY-1] blackOutCoordinates && notElem [curX,curY-1] currentPath  = mazeSolver maze curX (curY-1) blackOutCoordinates ([[curX,curY]] ++ currentPath ) startX startY (foundPath++['U'])
    | curY + 1 < length(maze) && ((maze !! (curY+1))!! curX)/='#' && notElem [curX,curY+1] blackOutCoordinates && notElem [curX,curY+1] currentPath  = mazeSolver maze curX (curY+1) blackOutCoordinates ([[curX,curY]] ++ currentPath ) startX startY (foundPath++['D'])
    | curX + 1 < (length (maze !! curY )) && ((maze !! curY)!! (curX+1))/='#'&& notElem [curX+1,curY] blackOutCoordinates && notElem [curX+1,curY] currentPath = mazeSolver maze (curX+1) curY blackOutCoordinates ([[curX,curY]] ++ currentPath ) startX startY (foundPath++['R'])
    | curX - 1 >= 0 && ((maze !! curY)!! (curX-1))/='#'&& notElem [curX-1,curY] blackOutCoordinates && notElem [curX-1,curY] currentPath = mazeSolver maze (curX-1) curY blackOutCoordinates ([[curX,curY]] ++ currentPath ) startX startY (foundPath++['L'])
    |otherwise = mazeSolver maze startX startY ([[curX,curY]] ++ blackOutCoordinates ) [] startX startY []

