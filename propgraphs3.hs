import Control.Monad
import System.IO  

type Lab = String
type Prop = String
data Val = Int | Double | String | Bool 
data V = V {vid::String, lab::Lab}
data E = E String Lab (V) (V) 
--data Label a = LabV (V a, Lab) | LabE (E a, Lab)

data PG = PG {vertices::[V], edges::[E]}
--
create :: PG
create = PG [] []  

addEdge :: PG -> String -> V -> V -> PG
addEdge (PG {vertices = v, edges = e}) e1 v1 v2 = 
        PG {vertices = (v1:v2:v), edges = ((E e1 "" v1 v2):e)}


addVertex :: PG -> String -> Lab -> PG
addVertex (PG {vertices=v, edges=e}) vid lab = 
    PG { vertices = removeRepeated $ insert v vid, edges=e} 

-- 
--QSORT
part :: String -> [String] -> ([String], [String])
part _ [] = ([],[])
part n (head:tail)
    | n < head = (l1, head:l2)
    | otherwise = (head:l1, l2)
            where (l1, l2) = part n tail

qsort :: [String] -> [String]
qsort [] = []
qsort [l] = [l]
qsort (head:tail) = qsort(fst (part head tail)) ++ [head] ++ qsort(snd(part head tail))

--INSERT 
insert :: [String] -> String -> [String]
insert [] n = [n]
insert (head:tail) n
    | head > n = n:head:tail
    | otherwise = head:(insert tail n)

--FLATTEN
flatten :: [String] -> String 
flatten [] = []
flatten [fin] = fin
flatten (head:tail) = head ++ flatten (tail)

--REMOVE REPEATED VERTICES
removeRepeated :: [String] -> [String]
removeRepeated [] = []
removeRepeated (x:xs) = [x] ++ removeRepeated (dropWhile(==x) xs) 

showVertices :: [V] -> [String] 
showVertices [] = [] 
showVertices ((V {vid=vid, lab=lab}):v) = [vid ++ "[" ++ lab ++ "]" ++ "\n"] ++ showVertices v


showGraph :: PG -> IO ()
showGraph (PG { vertices=v, edges=e }) = do 
        putStrLn $ flatten $ removeRepeated $ qsort $ showVertices v 
--
--(E s v1 v2)
--((V {vid=vid, lab=lab}):v)
----PG (V (v1:v2:a)) (E (e:b)) Label c 
----addEdge :: PG a -> E a -> V a -> V a -> PG a
----addEdge pg e v1 v2 = pg {vertices = [v1], edges = [e], labels = []}
--
main = do  
        let list = []
        rhoContents <- readFile "rhoFile.pg" 
        lambdaContents <- readFile "lambdaFile.pg"

--        let singlewords = words contents
--        let firstWord = returnFirst singlewords 
--        let newWords = removeFirst singlewords
        let pgraph = create
        let graf = foldl (process) pgraph $ lines rhoContents
        showGraph graf
        return()
       -- let secondWord = returnFirst newWords 
       -- let newWords2 = removeFirst newWords
       -- let thirdWord = returnFirst newWords2
       -- --let item = V "e1"
       -- let item = addEdge (create) ("e1") (V { vid="v1", lab="" }) (V { vid="v2", lab="" })
       -- showGraph item
       -- print firstWord
--        print secondWord
--        print thirdWord



first:: [PG] -> PG
first (pg1:pg:pgs) = pg

process :: PG -> String -> PG
process pg line = 
    addEdge pg (returnFirst ws) (V {vid=returnSnd ws, lab=""})  (V {vid=returnThird ws, lab=""})
    where 
        ws = words line

processData :: String -> [IO ()]
processData contents 
    = map (processLine) $ lines contents
           where
               processLine :: String -> IO()
               processLine line = putStrLn line   

returnFirst [] = ""
returnFirst [word] = word
returnFirst (word:words) = word

returnSnd [] = ""
returnSnd [word] = word
returnSnd (word1:word2:words) = word2

returnThird [] = ""
returnThird [word] = word
returnThird (word1:word2:word3:words) = word3

removeFirst [] = []
removeFirst [_] = []
removeFirst (first:words) = words

--populate :: String -> String -> String -> String -> PG
--https://stackoverflow.com/questions/25160308/complex-data-structures-in-haskell
