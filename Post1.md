# 用Haskell求解扑克牌残局

之前用python写过一个残局求解的脚本，但那个不优雅，这次Haskell重新实现一遍。

## 为什么要用Haskell

首先，Haskell是一个纯函数式编程语言。和传统的命令式（imperative）语言（C++/Java）相比，Haskell能让人把更多的脑力放到解决问题本身上。写代码的过程就是定义问题的过程，问题严谨地定义完了，解法也就出来了。所以Haskell非常适合数学上的问题求解。

此外，Haskell还有惰性求值（lazy evaluation）的特点。定义问题的时候可以使用嵌套以及定义无限的数据结构，只要最终求值是从中取有限值即可，极大地简化了定义问题的复杂度。

还有，Haskell的语法非常简洁，函数调用（应该叫函数应用function apply）甚至连括号都不需要，相比Java这些语言来说，极大减小了代码量。

最后，使用Haskell的原因，当然就是因为Haskell是世界上最好的语言了呀。

鉴于大家可能对Haskell的语法不太熟悉，后面我在描述解法的时候也会顺带讲解一下Haskell的语法。

## 数据类型定义

```haskell
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
```

前两行不用多说。因为代码比较简单我就放在一个Main.hs里面了。解法里面会用到一些List第函数，所以需要导入Data.List

首先定义花色。等号左边第Suite是类型名，右边“|”隔开的是多个可能的结构类型，分别表示黑桃/红桃/方片/梅花。结构类型同时也是其构造函数，构造函数后面可以跟着其他数据类型的字段，当然也可以没有其他字段，比如这里的花色。

每张牌有点数和花色两个属性，这里Card类型的定义是另一种叫做Record的方式，它同时为每个字段定义了一个用来提取其值的函数，把这个函数应用于某个Card值，就能获得其字段值。不过这种模式要求该数据类型只有一个结构类型。

Shape是一次可以出的牌型。现在仅支持出单张和对子。这里把不要也当作一个牌型，以简化分析建模过程。对于单张和对子，关注的信息都是点数以及其包含的卡牌的列表。

为了模拟出牌的过程，这里建立了一个Player类型，每个Player包含了其id，手上剩余的所有可能的牌型，以及手上剩余的所有卡牌。谁手上的剩余卡牌为空就赢了。

PlayTreeNode就是表示出牌过程的树。每个节点记录了当前玩家（playerIdInTree）当时所出的牌型（Shape）。树的每一层就代表了同一个玩家当前所有可以作出的出牌选择，并且相邻两层节点的是不同玩家。树的子节点列表就是玩家打出当前牌型的情况下，对方玩家的所有应对方式。整颗树就包含了两个玩家的所有出牌可能。而正确的解法就是这棵树的一个子树。

上面数据类型定义中的```deriving (Show,Eq)```里面的Show和Eq都是typeclass，类似于golang的interface。一个类型声明了属于某个typeclass之后，就表明这个typeclass的方法可以应用于该类型。比如属于Show的类型就可以应用到show方法，用来将该类型的值转化为字符串；属于Eq的类型就可以应用于==或者/=操作符来判断是否相等。这里deriving表明使用的是typeclass的默认实现，比如show的默认实现就是数据结构原样输出。typeclass的实现也可以自定义：

```haskell
instance Show Card where
    show (Card r s) = show r ++ show s

instance Show Shape where
    show Pass = "Pass"
    show (Single i cs) = "Single "  ++ show cs
    show (Double i cs) = "Double "  ++ show cs
```

比如输出卡牌的时候，我可以自定义show来把点数和花色连在一起现实，去掉空格。而牌型的显示就直接显示构造函数名和对应的卡牌列表，同时把点数隐藏了，因为卡牌列表已经有点数了。

注意到Shape的show函数用到了Haskell里面的pattern matching。show函数的类型是接收一个Shape类型，返回一个String类型。等号左边是函数名+参数Shape，右边是函数定义，也就是一个String表达式。因为Shape有3种取值类型，show函数也分3种实现。匹配时从上往下，匹配成功就退出。

## 从字符串生成牌型

首先我们需要能根据一副手牌解析出其所有可以组成的牌型。

### 字符串到卡牌列表

第一步是从一个字符串中解析出一副手牌。比如"9S 9H 6D 5C"就解析成♠️9，♥️9，♦6和♣️5这4张牌。

```haskell
parseCard :: String -> Card
parseCard str = let r = read (init str) :: Int
                    s = case last str of
                             'S' -> S
                             'H' -> H
                             'D' -> D
                             'C' -> C
                in Card r s
```

看parseCard函数。第一行是函数类型声明，后面的是函数体。等号右边最下边的`Card r s`才是函数的值，let语句中的是中间表达式。`r = read (init str) :: Int`表示将str字符串除最后一个字符意外的部分当作一个整数读取绑定到r。后面几行则是将最后一个字符读取识别为花色。

```haskell
parseCardString :: String -> [Card]
parseCardString = map parseCard.words

```

parseCardString函数则是先把字符串先按空白字符分割，然后逐个识别为卡牌，最后返回一个卡牌列表。注意上面`map parseCard.words`中的结合优先级是`(map parseCard).words`。因为haskell中函数应用（function apply）的优先级最高，所以去掉括号也可以。这里可以从右往左看，words函数先把String转化成[String]，然后map会把parsesCard函数逐个应用到[String]的每个元素，得到[Card]。

### 卡牌列表到牌型列表

接下来是从卡牌列表中枚举所有可能组成的牌型。因为随着对局的进行，每个选手的牌型数量是会不断被用出或者是拆散的，所以这是个一次性的过程。

因为我们暂时只考虑单张和对子两种牌型，所以枚举牌型的过程可以这样：

* 先把相同点数的卡牌归到一起，形成若干组。
* 然后在每个组中，取出1张的排列组合就是所有的单张牌型；取出2张的排列组合就是对子的牌型。

```haskell

equalRank :: Card -> Card -> Bool
equalRank c1 c2 = rank c1 == rank c2

groupByRank :: [Card] -> [[Card]]
groupByRank = groupBy equalRank

```

一个Data.List的库函数groupBy刚好可以做到这一点。它需要提供一个判断元素相等的函数。所以equalRank用来对比卡牌的点数来判断是否相等。

```haskell

combinations :: Int -> [a] -> [[a]]
combinations n cs = [c | c <- combs n cs, length c == n]
    where combs _ [] = [[]]
          combs 0 _  = [[]]
          combs k (x:xs) = x_start ++ others
            where x_start = [ x : rest | rest <- combs (k-1) xs ]
                  others  = if k <= length xs then combs k xs else []

```

combinations函数就是从一个列表[a]中取出n个元素的排列组合。这里的第二个参数类型是[a]而不是[Card]。这也是haskell中的范型编程，a是一个类型变量，可以是任意类型。

combinations的值就是一个list comprehension，用过python的同学都知道。通过一个中间函数combs也是得到一个组合的列表，然后只把长度刚好为n的列表加到结果列表中。

对于combs中间函数，有3个模式。前两个是边界条件，后面是用分治法实现的排列组合。从一个列表中抽取k元素的所有方法有两类：

* 选取列表的第一个元素，再从剩下的列表中选k-1个元素。
* 不选取列表的第一个元素，直接从剩下但列表中选择k个元素。

接下来就是从卡牌的列表的列表，生成牌型的列表。

```haskell

getSingles :: [[Card]] -> [Shape]
getSingles [] = []
getSingles (g:gs) = [Single (rank c) [c] | c <- g] ++ getSingles gs

getDoubles :: [[Card]] -> [Shape]
getDoubles [] = []
getDoubles (g:gs) = [Double (rank c) cs | cs@(c:_) <- combinations 2 g] ++ getDoubles gs
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- getDoubles = foldr (\ g -> (++) [Double (rank c) cs | cs@(c : _) <- combinations 2 g]) []

initPlayer :: Int -> String -> Player
initPlayer pId str = let cards = parseCardString str
                         cardGroups = groupByRank cards
                         shapes = getSingles cardGroups ++ getDoubles cardGroups
                     in Player pId shapes cards
```

getSingles函数第二个模式中的g:gs，g表示第一个元素，gs表示列表剩下的元素。这里g本身也是一个[Card]，可以生成一组牌型[Shape]。解决完第一个元素g之后，再把getSingles函数应用到剩下的列表，最终把所有[Shape]拼接成一个大的[Shape]。

这里还可以使用折叠（fold）来让代码更加紧凑，Haskell的lint工具就一直提示我用foldr。但这种方式理解起来可能有点吃力，有兴趣的话可以结合上面注释列出来foldr的函数类型来帮助理解。

最终我们从字符串开始，识别成卡牌列表，再得到牌型列表，最后放到来玩家类型Player里面。

## 构建出牌树

```haskell
buildPlayTree :: Player -> Player -> PlayTreeNode
buildPlayTree playerA playerB
    = PlayTreeNode 0 Pass (buildNextLevel playerA playerB)
        where buildNextLevel self@(Player pId shapes _) opponent = [PlayTreeNode pId s newNextLevelNodes | s <- shapes,
                let newSelf = updatePlayer self s,
                let curFinish = null (remainingCards newSelf),
                let newNextLevelNodes = if curFinish then [] else buildNextLevel opponent newSelf]
                    ++ [PlayTreeNode pId Pass (buildNextLevel opponent self)]
```

buildPlayTree函数接收两个Player参数（内部包含了玩家所有卡牌和可能的牌型），返回一个出牌树。这里我用一个假的节点作为根节点（id为0，牌型为Pass，而正常玩家的出牌节点为1或者2）以便把出牌树的森林统一成一棵树。从假根节点的子节点开始才是真正的出牌过程。

buildNextLevel函数是一个可以递归调用的内部函数（在where里定义）。因为构建树的当前这一层时只和第一个玩家的牌有关，所以只会模式匹配第一个玩家。表达式`self@(Player pId shapes _)`中@后面用于把Player的内部数据展开绑定到新字段上，@前面的self还是整个Player的值。opponent字段只是用于构建更下一级的节点时使用，不用展开。

buildNextLevel函数等号右边的值实际上是一个跨越4行的list comprehension，再加上一个单元素的list。用过python的同学应该都比较熟悉这里list comprehension的语法，这里就是针对当前玩家每一个可能的牌型（Shape）都生成一个子树节点，而3个let语句是每一个牌型都执行一遍，用来生成新子节点的子节点列表。

updatePlayer函数用于计算出了当前的牌型之后，删除相关的牌型和卡牌，返回一个新的Player，以用于下一层的树。

curFinish是判断当前节点是否是叶子节点。如果当前玩家已经没有手牌，就不然对方继续出牌，因而是叶子节点，牌局结束。

newNextLevelNodes是下一级子节点列表。如果当前节点还不是叶子节点，那交换玩家的位置，构建下一级子节点。

最后还要加上一个Pass子节点，因为除了把手里的牌打出来，玩家还可以不要。

```haskell
updatePlayer :: Player -> Shape -> Player
updatePlayer (Player pId shapes cards) shapeDeleted = Player pId newShapes newCards
    where newCards = [ c | c <- cards, c `notElem` getCards shapeDeleted]
          newShapes = [ s | s <- shapes, s /= shapeDeleted, not (s `shapeIntersected` shapeDeleted)]
```

updatePlayer实际是更新Player中的shapes和cards，然后返回一个新的Player值。组成刚打出的牌型（shapeDeleted）的所有卡牌都要去掉；所有和这个牌型相交（包含至少一个相同卡牌）的牌型也要去掉。

```haskell
shapeIntersected :: Shape -> Shape -> Bool
shapeIntersected s1 s2 = not (null $ getCards s1 `intersect` getCards s2)

getCards :: Shape -> [Card]
getCards Pass = []
getCards (Single _ cards) = cards
getCards (Double _ cards) = cards
```

intersect函数是List标准库提供的函数，用于判断两个list是否有相同的元素。

至此出牌树就构建出来了。但是这颗树包含了所有不可获胜的以及不合理的出牌情况。并且，这颗树还是无限的，显然不能当作解法树。

## 生成解法树

思路是这样的：利用Haskell的惰性求值（lazy evaluation）特性，先把包含了解法的集合（也就是出牌树）定义出来，这时候的集合可能非常庞大甚至无限大，但是因为还没有求值并不占用很大空间；然后逐步把不需要的元素排除掉，过滤后的集合才是解法集。

### 清理无效分支

能压过对方的牌行才能出，所以一个节点的所有子节点的牌型都应该“大于”自己。先定义greater函数

```haskell
greater :: Shape -> Shape -> Bool
greater (Single r1 _) (Single r2 _) = r1 > r2
greater (Double r1 _) (Double r2 _) = r1 > r2
greater Pass Pass = False
greater Pass _ = True
greater _ Pass = True
greater _ _ = False
```

注意模式匹配是从上到下进行的，一旦匹配成功就退出了，对应分支的值就是函数的值。
1-2行：相同牌型的时候，直接对比点数。
3行：Pass不能大于Pass，毕竟对方不要的时候自己必须出牌。
4-5行：Pass可以压过任何其他牌，其他任何牌都可以压过Pass。
6行：不相同的牌型的牌不能压。

```haskell
pruneValid :: PlayTreeNode -> PlayTreeNode
pruneValid (PlayTreeNode pId s subTrees) = PlayTreeNode pId s [pruneValid t | t <- subTrees, shape t `greater` s]
```

pruneValid函数接收一颗树，返回一颗新的树，其中所有子树节点都大于父节点。这里可以看到普通函数也可以用backstick操作符抱起来，作为操作符使用，这样更好理解。

### 判断是否能赢

现在的出牌树已经是合理的出牌树了，接下来就要挑选出能赢的子树。

```haskell
nodeWin :: PlayTreeNode -> Bool
nodeWin (PlayTreeNode _ _ []) = True
nodeWin (PlayTreeNode _ _ subTrees) = all (==False) $ map nodeWin subTrees

playerWin :: PlayTreeNode -> Int -> Bool
playerWin (PlayTreeNode _ _ subTrees@(t:_)) pId
            | playerIdInTree t == pId = foldr ((||).nodeWin) False subTrees
            | otherwise = all (==False) $ map nodeWin subTrees
```

任意给定某个节点，我需要能判断当前玩家出牌出到这里的时候，再往下是否有必胜的策略。显然如果本节点是叶子节点，那就已经胜利了；如果是非叶子节点，那必胜的条件是其所有子节点都是失败的。

判断一个玩家能否必胜的时候稍有区别。因为跟节点是虚假的，出牌过程从第二层开始，所以playerWin函数要把子树展开。如果第二层就是当前玩家出牌，那只要这一层中有一颗子树能获胜就行；否则，就需要这层的所有子树都失败才行。

### 过滤出解法树

只是判断谁能赢还不够，给出赢的过程才有意思。

```haskell
keepAllSubTree :: PlayTreeNode -> PlayTreeNode
keepAllSubTree p@(PlayTreeNode _ _ []) = p
keepAllSubTree (PlayTreeNode i s subTrees) = PlayTreeNode i s $ map keepOneSubTree subTrees

keepOneSubTree :: PlayTreeNode -> PlayTreeNode
keepOneSubTree (PlayTreeNode i s subTrees) = let t1 = head [keepAllSubTree st | st <- subTrees, nodeWin st]
                                             in PlayTreeNode i s [t1]
```

因为我还没有做等效牌型的过滤（那是另外一篇文章了），解法树还是挺大的，看起来不方便。这里我暂时偷懒，每次只选择第一个胜利分支，当然对应对方的所有分支都得保留。

### 显示解法树

这里我直接把Data.List里面的标准函数copy出来改了一下：

```haskell
draw :: PlayTreeNode -> [String]
draw root@(PlayTreeNode pId shape ts0) = ("Player " ++ show pId ++ " " ++ show shape ++ " Win:" ++ show (nodeWin root)) : drawSubTrees ts0
    where drawSubTrees [] = []
          drawSubTrees [t] = "|" : shift "`- " "   " (draw t)
          drawSubTrees (t:ts) = "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
          shift first other = zipWith (++) (first : repeat other)

drawTree :: PlayTreeNode -> String
drawTree  = unlines . draw
```

### main函数

这里我暂时直接把测试的牌型写在代码里了。先出牌的玩家A是♥️9，♥️6，♦️6，♥️4，♠️3，后出牌的玩家B是♠️8和♥️8。我们先判断是哪个玩家能赢，如果先出牌玩家能赢，则只需要保留第一个能赢的子树即可；如果后出牌玩家能赢，此时能赢玩家第一次出牌是在树的第三层了，第二层还是对方玩家出牌，所以需要保留第二层所有对方玩家的出牌子树。

```haskell
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
```

## 解法显示

```
A: 9H 6H 6D 4H 3S
B: 8S 8H
Winner: A
Player 0 Pass Win:False
|
`- Player 1 Single [4H] Win:True
   |
   +- Player 2 Single [8S] Win:False
   |  |
   |  `- Player 1 Single [9H] Win:True
   |     |
   |     `- Player 2 Pass Win:False
   |        |
   |        `- Player 1 Double [6H,6D] Win:True
   |           |
   |           `- Player 2 Pass Win:False
   |              |
   |              `- Player 1 Single [3S] Win:True
   |
   +- Player 2 Single [8H] Win:False
   |  |
   |  `- Player 1 Single [9H] Win:True
   |     |
   |     `- Player 2 Pass Win:False
   |        |
   |        `- Player 1 Double [6H,6D] Win:True
   |           |
   |           `- Player 2 Pass Win:False
   |              |
   |              `- Player 1 Single [3S] Win:True
   |
   `- Player 2 Pass Win:False
      |
      `- Player 1 Single [3S] Win:True
         |
         +- Player 2 Single [8S] Win:False
         |  |
         |  `- Player 1 Single [9H] Win:True
         |     |
         |     `- Player 2 Pass Win:False
         |        |
         |        `- Player 1 Double [6H,6D] Win:True
         |
         +- Player 2 Single [8H] Win:False
         |  |
         |  `- Player 1 Single [9H] Win:True
         |     |
         |     `- Player 2 Pass Win:False
         |        |
         |        `- Player 1 Double [6H,6D] Win:True
         |
         `- Player 2 Pass Win:False
            |
            `- Player 1 Single [9H] Win:True
               |
               `- Player 2 Pass Win:False
                  |
                  `- Player 1 Double [6H,6D] Win:True
```
