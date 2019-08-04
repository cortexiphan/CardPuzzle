module Main where
import Data.List

-- S:Spade, H:Heart, D:Diamand, C:Club
data Suite = S | H | D | C deriving (Show,Eq)
data Card = Card {rank :: Int, suite :: Suite} deriving (Eq)
data Shape = Pass | Single Int [Card] | Double Int [Card] deriving (Eq)
data Player = Player {
    playerId :: Int,
    remainingShapes :: [Shape],
    remainingCards :: [Card]
} deriving (Show)
data PlayTreeNode = PlayTreeNode{
    playerIdInTree :: Int,
    shape :: Shape,
    nextLevelNodes :: [PlayTreeNode]
} deriving (Show)


instance Show Card where
    show (Card r s) = show r ++ show s

instance Show Shape where
    show Pass = "Pass"
    show (Single i cs) = "Single "  ++ show cs
    show (Double i cs) = "Double "  ++ show cs

parseCard :: String -> Card
parseCard str = let r = read (init str) :: Int 
                    s = case last str of
                             'S' -> S
                             'H' -> H
                             'D' -> D
                             'C' -> C
                in Card r s

parseCardString :: String -> [Card]
parseCardString = map parseCard.words

equalRank :: Card -> Card -> Bool
equalRank c1 c2 = rank c1 == rank c2

groupByRank :: [Card] -> [[Card]]
groupByRank = groupBy equalRank

combinations :: Int -> [a] -> [[a]]
combinations n cs = [c | c <- combs n cs, length c == n]
    where combs _ [] = [[]]
          combs 0 _  = [[]]
          combs k (x:xs) = x_start ++ others
            where x_start = [ x : rest | rest <- combs (k-1) xs ]
                  others  = if k <= length xs then combs k xs else []


getSingles :: [[Card]] -> [Shape]
-- getSingles [] = []
-- getSingles (g:gs) = [Single (rank c) [c] | c <- g] ++ getSingles gs
getSingles = foldr (\ g -> (++) [Single (rank c) [c] | c <- g]) []
-- foldr :: (a -> b -> b) -> b -> [a] -> b

getDoubles :: [[Card]] -> [Shape]
-- getDoubles [] = []
-- getDoubles (g:gs) = [Double (rank c) cs | cs@(c:_) <- combinations 2 g] ++ getDoubles gs
getDoubles = foldr (\ g -> (++) [Double (rank c) cs | cs@(c : _) <- combinations 2 g]) []

greater :: Shape -> Shape -> Bool
greater (Single r1 _) (Single r2 _) = r1 > r2
greater (Double r1 _) (Double r2 _) = r1 > r2
greater Pass Pass = False
greater Pass _ = True
greater _ Pass = True
greater _ _ = False

getCards :: Shape -> [Card]
getCards Pass = []
getCards (Single _ cards) = cards
getCards (Double _ cards) = cards

shapeIntersected :: Shape -> Shape -> Bool
shapeIntersected s1 s2 = not (null $ getCards s1 `intersect` getCards s2)

updatePlayer :: Player -> Shape -> Player
updatePlayer (Player pId shapes cards) shapeDeleted = Player pId newShapes newCards
    where newCards = [ c | c <- cards, c `notElem` getCards shapeDeleted]
          newShapes = [ s | s <- shapes, s /= shapeDeleted, not (s `shapeIntersected` shapeDeleted)]

buildPlayTree :: Player -> Player -> PlayTreeNode
buildPlayTree playerA playerB 
    = PlayTreeNode 0 Pass (buildNextLevel playerA playerB)
        where buildNextLevel self@(Player pId shapes _) opponent = [PlayTreeNode pId s newNextLevelNodes | s <- shapes, 
                let newSelf = updatePlayer self s, 
                let curFinish = null (remainingCards newSelf), 
                let newNextLevelNodes = if curFinish then [] else buildNextLevel opponent newSelf]
                    ++ [PlayTreeNode pId Pass (buildNextLevel opponent self)]

initPlayer :: Int -> String -> Player
initPlayer pId str = let cards = parseCardString str
                         cardGroups = groupByRank cards
                         shapes = getSingles cardGroups ++ getDoubles cardGroups
                     in Player pId shapes cards

draw :: PlayTreeNode -> [String]
draw root@(PlayTreeNode pId shape ts0) = ("Player " ++ show pId ++ " " ++ show shape ++ " Win:" ++ show (nodeWin root)) : drawSubTrees ts0
    where drawSubTrees [] = []
          drawSubTrees [t] = "|" : shift "`- " "   " (draw t)
          drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
          shift first other = zipWith (++) (first : repeat other)

drawTree :: PlayTreeNode -> String
drawTree  = unlines . draw

-- Manipulating tree
pruneValid :: PlayTreeNode -> PlayTreeNode
pruneValid (PlayTreeNode pId s subTrees) = PlayTreeNode pId s [pruneValid t | t <- subTrees, shape t `greater` s]

takeNLevelTree :: Int -> PlayTreeNode -> PlayTreeNode
takeNLevelTree n (PlayTreeNode pId shape subTrees)
            | n <= 1 || null subTrees = PlayTreeNode pId shape []
            | n > 1  = PlayTreeNode pId shape $ map (takeNLevelTree (n - 1)) subTrees

nodeWin :: PlayTreeNode -> Bool
nodeWin (PlayTreeNode _ _ []) = True
nodeWin (PlayTreeNode _ _ subTrees) = all (==False) $ map nodeWin subTrees

playerWin :: PlayTreeNode -> Int -> Bool
playerWin (PlayTreeNode _ _ subTrees@(t:_)) pId
            | playerIdInTree t == pId = foldr ((||).nodeWin) False subTrees
            | otherwise = all (==False) $ map nodeWin subTrees

keepAllSubTree :: PlayTreeNode -> PlayTreeNode
keepAllSubTree p@(PlayTreeNode _ _ []) = p
keepAllSubTree (PlayTreeNode i s subTrees) = PlayTreeNode i s $ map keepOneSubTree subTrees

keepOneSubTree :: PlayTreeNode -> PlayTreeNode
keepOneSubTree (PlayTreeNode i s subTrees) = let t1 = head [keepAllSubTree st | st <- subTrees, nodeWin st]
                                               in PlayTreeNode i s [t1]
          
main :: IO ()
main = do
    let playerA = initPlayer 1 "9H 6H 6D 4H 3S"
        playerB = initPlayer 2 "8S 8H"
        tree = buildPlayTree playerA playerB
        validTree = pruneValid tree
        aWin = playerWin validTree 1
        solutionTree = if aWin then keepOneSubTree validTree else keepAllSubTree validTree
    putStr $ "Winner: " ++ (if aWin then "A" else "B") ++ "\r\n"
    putStr $ drawTree solutionTree
