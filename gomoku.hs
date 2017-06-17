import Data.List
import Data.Tree

data Field = 
    NewLine
    | Empty
    | Cross
    | Circle
    deriving Eq

instance Show Field where
    show NewLine = "\n"
    show Empty = "_"
    show Cross = "x"
    show Circle = "o"
             
data Board = Board [[Field]] 
    
instance Show Board where
    show ( Board fields ) = 
        show $ toShowList fields     

toShowList :: [[Field]] -> [Field]
toShowList board = 
    intercalate [NewLine] board

createRow :: Int -> [Field]   
createRow size =
    take size ( repeat Empty )
    
createBoard :: Board
createBoard =
    Board $ take 19 $ repeat $ take 19 $ repeat Empty

insertCircle :: Int -> Int -> Board -> Board  
insertCircle x y board =
    insertFigure x y board Circle
{-
o :: (Int,Int) -> Board -> Board    
o (x,y) board =
    insertCircle x y board
-}
insertCross :: Int -> Int -> Board -> Board     
insertCross x y board =
    insertFigure x y board Cross
{-
x :: (Int,Int) -> Board -> Board    
x (xx,y) board =
    insertCross xx y board  
-}  
insertFigure :: Int -> Int -> Board -> Field -> Board  
insertFigure x y (Board board) figure = 
    Board $
        take (y-1) board ++
        [ concat ( 
            [ take (x-1) ( board !! (y-1) ) ] ++ 
            [[figure]] ++  
            [ drop x ( board !! (y-1) ) ] 
          ) ] ++   
        drop y board
        
getFigure :: Int -> Int -> Board -> Field        
getFigure x y (Board board) =
    ( head ( drop (y-1) board ) ) !! (x-1)
    
isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y board = 
    getFigure x y board == Empty 
    
toList :: Board -> [Field]    
toList (Board fields) = 
    intercalate [] fields
    
isInList :: Int -> Field -> Board -> Bool 
isInList i figure board  =
     ( getFigure ( (i `mod` 19) + 1) ( (i `div` 19) + 1 ) board ) == figure
    
getIndexesList :: Int -> Field -> Board -> [Int]    
getIndexesList 361 _ _ = []
getIndexesList i figure board
    | isInList i figure board = i : getIndexesList (i+1) figure board
    | otherwise = getIndexesList (i+1) figure board   
 
getCirclesIndexList :: Board -> [Int]    
getCirclesIndexList board =
    getIndexesList 0 Circle board
 
getCrossesIndexList :: Board -> [Int]      
getCrossesIndexList board =
    getIndexesList 0 Cross board

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

isArithmeticSequence :: [Int] -> Bool
isArithmeticSequence [] = False
isArithmeticSequence [x] = True     --na potrzeby wyszukiwania pojedynczych zapelnionych pol
isArithmeticSequence [x,y] = True
isArithmeticSequence (x:y:z:xs) = (x - y) == (y - z) && isArithmeticSequence (y:z:xs)  

findArithmeticSequence :: Int -> Int -> [Int] -> [Int] 
findArithmeticSequence x0 diff list =
    intersect [x0,x0+diff..x0+diff*1000] list

findAllSequencesOfLength :: Int -> Int -> [Int] -> [[Int]]
findAllSequencesOfLength _ _ [] = []    
findAllSequencesOfLength len diff list@(h:tail)
    | (length list >= len) 
        = (sublists 0) ++ findSequencesOfLength len diff tail  
    | otherwise = []
    where sublist x0 = take len (findArithmeticSequence x0 diff list)
          sublists 360 = []
          sublists x
              | ((length (sublist x)) == len) && (isArithmeticSequence $ sublist x) =
                  sublist x : sublists (x+1)
              | otherwise = sublists (x+1)

findSequencesOfLength :: Int -> Int -> [Int] -> [[Int]]  
findSequencesOfLength len diff list =
    removeDuplicates $ findAllSequencesOfLength len diff list
    
checkIfGameOver :: Board -> Bool    
checkIfGameOver board
    | findSequencesOfLength 5 1 crossList /= [] = True
    | findSequencesOfLength 5 19 crossList /= [] = True
    | findSequencesOfLength 5 20 crossList /= [] = True
    | findSequencesOfLength 5 18 crossList /= [] = True
    | findSequencesOfLength 5 1 circleList /= [] = True
    | findSequencesOfLength 5 19 circleList /= [] = True
    | findSequencesOfLength 5 20 circleList /= [] = True
    | findSequencesOfLength 5 18 circleList /= [] = True
    | otherwise = False
    where crossList = getCrossesIndexList board
          circleList = getCirclesIndexList board
    
test1 = insertCross 1 1 $ insertCross 1 2 $ insertCross 1 3 $ insertCross 1 4 $ insertCross 1 5 createBoard
test2 = insertCircle 1 1 $ insertCircle 2 1 $ insertCircle 3 1 $ insertCircle 4 1 $ insertCircle 5 1 createBoard
test3 = insertCross 1 1 $ insertCross 2 2 $ insertCross 3 3 $ insertCross 4 4 $ insertCross 5 5 createBoard    
test4 = insertCross 5 1 $ insertCross 4 2 $ insertCross 3 3 $ insertCross 2 4 $ insertCross 1 5 createBoard    
test5 = insertCircle 5 1 $ insertCross 4 2 $ insertCross 3 3 $ insertCircle 2 4 $ insertCross 1 5 createBoard     
    
--Assuming that AI plays with Crosses, arg before * is weight   
rankBoard :: Board -> Int 
rankBoard board =
    1000 * length ( findSequencesOfLength 5 1 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 1 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 1 $ getCrossesIndexList board ) +
    1000 * length ( findSequencesOfLength 5 19 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 19 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 19 $ getCrossesIndexList board ) +
{-    1000 * length ( findSequencesOfLength 5 18 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 18 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 18 $ getCrossesIndexList board ) +
    --2 * length ( findSequencesOfLength 2 18 $ getCrossesIndexList board ) +
    1000 * length ( findSequencesOfLength 5 20 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 20 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 20 $ getCrossesIndexList board ) +
       -----------------------------------------------------------------
    1000 * length ( findSequencesOfLength 5 1 $ getCirclesIndexList board )-} 
    10 * length ( findSequencesOfLength 4 1 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 1 $ getCirclesIndexList board ) -
    1000 * length ( findSequencesOfLength 5 19 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 19 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 19 $ getCirclesIndexList board ) 
{-    1000 * length ( findSequencesOfLength 5 18 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 18 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 18 $ getCirclesIndexList board ) -
    1000 * length ( findSequencesOfLength 5 20 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 20 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 20 $ getCirclesIndexList board ) 
-}            
test6 = o 7 6 $ x 8 6 $ o 8 5 $ x 7 5 $ o 9 4 $ x 10 3 $ o 9 6 $ x 6 4 $
        o 9 7 $ x 9 5 $ o 10 7 $ x 7 4 $ o 8 7 $ x 5 3 $ o 4 2 $ x 11 7 $
        o 6 7 createBoard
        where o = insertCircle
              x = insertCross

getNeighbours :: Int -> Int -> [(Int,Int)]
getNeighbours 1 1 = [ (1,2),(2,2),(2,1) ]
getNeighbours 19 1 = [ (18,1),(18,2),(19,2) ]
getNeighbours 1 19 = [ (1,18),(2,18),(2,19) ]
getNeighbours 19 19 = [ (18,19),(18,18),(19,18) ]
getNeighbours 1 y = [ (1,y-1),(2,y-1),(2,y),(2,y+1),(1,y+1) ]
getNeighbours x 1 = [ (x-1,1),(x-1,2),(x,2),(x+1,2),(x+1,1) ]
getNeighbours 19 y = [ (19,y-1),(18,y-1),(18,y),(18,y+1),(19,y+1) ]
getNeighbours x 19 = [ (x-1,19),(x-1,18),(x,18),(x+1,18),(x+1,19) ]
getNeighbours x y = [ (x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1) ]

isEmpty' :: Board -> (Int,Int) -> Bool
isEmpty' board (x,y) = isEmpty x y board

getEmptyNeighbours :: Board -> (Int,Int) -> [(Int,Int)]    
getEmptyNeighbours board (x,y) =
    filter (isEmpty' board) (getNeighbours x y)

getPossibleMoves :: Board -> [(Int,Int)]
getPossibleMoves board =
    removeDuplicates $ foldl ( ++ ) []
    [ getEmptyNeighbours board ( ((i `mod` 19)+1),((i `div` 19)+1) ) | i <- ((getCirclesIndexList board) ++ (getCrossesIndexList board) ) ]

buildGameTree :: Int -> Field -> Board -> Tree (Board, Int)
buildGameTree depth figure board
    | depth == 0 = Node (board,rankBoard board) []
    | otherwise = buildForest
    where buildForest
            | figure == Cross = 
                Node (board,rankBoard board) 
                [ buildGameTree (depth-1) Circle (insertCross x y board) | (x,y) <- getPossibleMoves board ]      
            | otherwise = 
                Node (board,rankBoard board)
                [ buildGameTree (depth-1) Cross (insertCircle x y board) | (x,y) <- getPossibleMoves board ]      

--Player playing Circles is looking for minimal board value, Crosses for maximal

generateMove' :: Tree (Board,Int) -> Board -> Field -> (Int,Int)
generateMove' gametree board Cross =
    case (elemIndex (maximum list) list) of
        Just n -> getPossibleMoves board !! n
        Nothing -> getPossibleMoves board !! 6
    where list = [ minimum m | m <- g ]
          g = map (map snd) ( get2Lev gametree )

generateMove' gametree board Circle =
    case (elemIndex (minimum list) list) of
        Just n -> getPossibleMoves board !! n
        Nothing -> getPossibleMoves board !! 6
    where list = [ maximum m | m <- g ]
          g = map (map snd) ( get2Lev gametree )


generateMove gametree board Circle =
    case (elemIndex (minimum c) c) of
        Just n -> (getPossibleMoves board) !! n
        Nothing -> (getPossibleMoves board) !! 0
    where c = map snd $ (levels gametree) !! 1
generateMove gametree board Cross =
    case (elemIndex (maximum c) c) of
        Just n -> (getPossibleMoves board) !! n
        Nothing -> (getPossibleMoves board) !! 0
    where c = map snd $ (levels gametree) !! 1     
          
test7 = insertCross 10 1 $ insertCross 10 2 $ insertCross 10 4 $ 
        insertCross 10 5 createBoard
        
get2Lev (Node val []) = []
get2Lev (Node v (x:xs)) =
    ((levels x) !! 1) : get2Lev (Node v xs) 

