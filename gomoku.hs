import Data.List
import Data.Tree
import System.Random
import System.IO.Unsafe

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

insertCross :: Int -> Int -> Board -> Board     
insertCross x y board =
    insertFigure x y board Cross
  
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
    intersect [x0,x0+diff..x0+diff*400] list

findAllSequencesOfLength :: Int -> Int -> [Int] -> [[Int]]
findAllSequencesOfLength _ _ [] = []    
findAllSequencesOfLength len diff list@(h:tail)
    | (length list >= len) 
        = (sublists 0) ++ findAllSequencesOfLength len diff tail                --All
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
    
rankBoard :: Board -> Int 
rankBoard board =
    1000 * length ( findSequencesOfLength 5 1 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 1 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 1 $ getCrossesIndexList board ) +
    2 * length ( findSequencesOfLength 2 1 $ getCrossesIndexList board ) +
    1000 * length ( findSequencesOfLength 5 19 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 19 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 19 $ getCrossesIndexList board ) +
    2 * length ( findSequencesOfLength 2 19 $ getCrossesIndexList board ) +
    1000 * length ( findSequencesOfLength 5 18 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 18 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 18 $ getCrossesIndexList board ) +
    2 * length ( findSequencesOfLength 2 18 $ getCrossesIndexList board ) +
    1000 * length ( findSequencesOfLength 5 20 $ getCrossesIndexList board ) +
    10 * length ( findSequencesOfLength 4 20 $ getCrossesIndexList board ) +
    5 * length ( findSequencesOfLength 3 20 $ getCrossesIndexList board ) +
    2 * length ( findSequencesOfLength 2 20 $ getCrossesIndexList board ) +
       -----------------------------------------------------------------
    1000 * length ( findSequencesOfLength 5 1 $ getCirclesIndexList board )- 
    10 * length ( findSequencesOfLength 4 1 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 1 $ getCirclesIndexList board ) -
    2 * length ( findSequencesOfLength 2 1 $ getCrossesIndexList board ) -
    1000 * length ( findSequencesOfLength 5 19 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 19 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 19 $ getCirclesIndexList board ) -
    2 * length ( findSequencesOfLength 2 19 $ getCrossesIndexList board ) -
    1000 * length ( findSequencesOfLength 5 18 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 18 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 18 $ getCirclesIndexList board ) -
    2 * length ( findSequencesOfLength 2 18 $ getCrossesIndexList board ) -
    1000 * length ( findSequencesOfLength 5 20 $ getCirclesIndexList board ) -
    10 * length ( findSequencesOfLength 4 20 $ getCirclesIndexList board ) -
    5 * length ( findSequencesOfLength 3 20 $ getCirclesIndexList board ) -
    2 * length ( findSequencesOfLength 2 20 $ getCrossesIndexList board ) 
{-
rankBoard board
    | findSequencesOfLength 5 1 crossList /= [] = 1000
    | findSequencesOfLength 5 19 crossList /= [] = 1000
    | findSequencesOfLength 5 20 crossList /= [] = 1000
    | findSequencesOfLength 5 18 crossList /= [] = 1000
    | findSequencesOfLength 5 1 circleList /= [] = -1000
    | findSequencesOfLength 5 19 circleList /= [] = -1000
    | findSequencesOfLength 5 20 circleList /= [] = -1000
    | findSequencesOfLength 5 18 circleList /= [] = -1000
    | otherwise = unsafePerformIO (getStdRandom (randomR (0, 100)))
    where crossList = getCrossesIndexList board
          circleList = getCirclesIndexList board
-}    
  
--rankBoard board =
--     unsafePerformIO (getStdRandom (randomR (0, 100)))
             
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
        Nothing -> getPossibleMoves board !! 0
    where list = [ minimum m | m <- g ]
          g = map (map snd) ( get2Lev gametree )

generateMove' gametree board Circle =
    case (elemIndex (minimum list) list) of
        Just n -> getPossibleMoves board !! n
        Nothing -> getPossibleMoves board !! 0
    where list = [ maximum m | m <- g ]
          g = map (map snd) ( get2Lev gametree )

generateMove :: Tree (Board,Int) -> Board -> Field -> (Int, Int)
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

get2Lev :: Tree a -> [[a]]        
get2Lev (Node val []) = []
get2Lev (Node v (x:xs)) =
    ((levels x) !! 1) : get2Lev (Node v xs) 

makeMove :: Field -> Board -> Board    
makeMove Circle board =
    insertCircle x y board  
    where gt = buildGameTree 2 Circle board
          (x,y) = generateMove' gt board Circle
          
makeMove Cross board =
    insertCross x y board  
    where gt = buildGameTree 2 Cross board
          (x,y) = generateMove' gt board Cross   
    
gameLoop :: Board -> Field -> IO()    
gameLoop board turn = do
    putStrLn ( show board )
    putStrLn "\n"
    if checkIfGameOver board then do
        putStrLn "Koniec gry"
    else do
        if turn == Cross then do
            let boardAfterMove = makeMove Cross board
            gameLoop boardAfterMove Circle
        else do
            let boardAfterMove = makeMove Circle board
            gameLoop boardAfterMove Cross
            
main :: IO ()
main = do
    let board = insertCircle (unsafePerformIO (getStdRandom (randomR (1, 19)))) 
                (unsafePerformIO (getStdRandom (randomR (1, 19)))) createBoard
    gameLoop board Cross
    
          

