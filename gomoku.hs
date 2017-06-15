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
    
ifEmpty :: Int -> Int -> Board -> Bool
ifEmpty x y board = 
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
isArithmeticSequence [x] = False
isArithmeticSequence [x,y] = True
isArithmeticSequence (x:y:z:xs) = (x - y) == (y - z) && isArithmeticSequence (y:z:xs)  

findArithmeticSequence :: Int -> [Int] -> [Int] 
findArithmeticSequence diff list =
    intersect [0,diff..diff*1000] list

findAllSequencesOfLength :: Int -> Int -> [Int] -> [[Int]]    
findAllSequencesOfLength len diff list@(h:tail)
    | (length list /= len) && (length sublist == len) && (isArithmeticSequence sublist)
        = sublist : findSequencesOfLength len diff tail  
    | otherwise = []
    where sublist = take len (findArithmeticSequence diff list)

findSequencesOfLength :: Int -> Int -> [Int] -> [[Int]]  
findSequencesOfLength len diff list =
    removeDuplicates $ findAllSequencesOfLength len diff list
            


