import Control.Monad
import System.IO  
import Data.Maybe

--Both labels and properties represented by a string
type Lab = String
type Prop = String

--Values can be empty (Nothing) or one of the following types
data Val = I (Maybe Int) | D (Maybe Double) | S (Maybe String) | B (Maybe Bool) 
    deriving(Eq)

--Sigma represented by a tuple of a property and a value
data Sigma = Sigma {prop::Prop, val::Val} 
    deriving(Eq)

--Every vertex has an id (name), a label, an array of sigmas and an array of its adjacent vertices
data V = V {vid::String, lab::Lab, propV::[Sigma], adjacency::[String]} 
    deriving(Eq)

--Every edge has an id (name), a label, two vertices ids that form the edge and an array of sigmas
data E = E {eid::String, labE::Lab, v1::String, v2::String, propE::[Sigma]}

--A PG is a property graph formed by an array of vertices and an array of edges
data PG = PG {vertices::[V], edges::[E]}



--Creates an empty property graph
create :: PG
create = PG [] []  

--Returns a Nothing value for every different type of value
processValue :: String -> Val
processValue val
    | val == "Double" = D Nothing
    | val == "Int" = I Nothing
    | val == "String" = S Nothing
    | val == "Bool" = B Nothing

--Adds every property with value nothing
processProperties :: [Sigma] -> String -> [Sigma]
processProperties prop line = [Sigma {prop=first, val=processValue snd}] ++ prop
    where
        (first:snd:_) = words line

--Adds an edge and its two vertices to the property graph
--  Also adds all the properties to the edge with value nothing
--  And adds the new adjacency to both of the vertices
addEdge :: PG -> String -> V -> V -> [String] -> PG
addEdge (PG { vertices = vertices, edges = edges }) e1 v1 v2 properties = do 
    let graphV = addVertex (PG { vertices=vertices, edges=edges }) v1 properties
    let (PG { vertices=vertices2, edges=_ }) = addVertex graphV v2 properties
    let verticesAdj = addAdjacencies vertices2 (returnVid v1) (returnVid v2)
    let (PG { vertices=vertices3, edges=_ })= PG { vertices=verticesAdj, edges=edges }
    let verticesAdj2 = addAdjacencies vertices3 (returnVid v2) (returnVid v1)
    let (PG { vertices=vertices4, edges=_ }) = PG { vertices=verticesAdj2, edges=edges } 

    let V { vid=vid1 } = v1
    let V { vid=vid2 } = v2
    PG { vertices = vertices4, edges = 
        (insertEdges edges (E {eid=e1, labE="", v1=vid1, v2=vid2, propE=(foldl (processProperties) [] properties)})) }

--Adds a vertex to the property graph 
--  all the properties initialized to nothing
--  empty label 
--  empty adjacencies
addVertex :: PG -> V -> [String] -> PG
addVertex (PG { vertices=vertices, edges=edges }) (V { vid=vid, lab=lab }) properties
    | isAlreadyAddedV vertices vid = PG { vertices=vertices, edges=edges }
    | otherwise = PG { vertices = insert vertices (V {vid=vid, lab="", propV=propsV, adjacency=[]}), edges=edges } 
        where
            propsV = foldl (processProperties) [] properties

--Given the array of vertices and two vertices names, adds second vertex to the adjacencies of the first
addAdjacencies :: [V] -> String -> String -> [V]
addAdjacencies (V { vid=vid, lab=lab, propV=propV, adjacency=adj }:vertices) v1 v2 
    | vid == v1 = (V { vid=vid, lab=lab, propV=propV, adjacency=(v2:adj) }:vertices)
    | otherwise = [V { vid=vid, lab=lab, propV=propV, adjacency=adj }] ++ addAdjacencies vertices v1 v2 

--Insert vertex in order
insert :: [V] -> V -> [V]
insert [] v = [v]
insert ((V { vid=vid, lab=lab, propV=propV, adjacency=adj }):tail) (V { vid=vid2, lab=lab2, propV=prop, adjacency=adj2 })
    | vid > vid2 = (V{ vid=vid2, lab=lab2, propV=prop, adjacency=adj2 }):V{ vid=vid, lab=lab, propV=propV, adjacency=adj }:tail
    | otherwise = 
        (V{ vid=vid, lab=lab, propV=propV, adjacency=adj }):(insert tail (V{ vid=vid2, lab=lab2, propV=prop, adjacency=adj2 }))
 
--Insert edge in order
insertEdges :: [E] -> E -> [E]
insertEdges [] e = [e]
insertEdges ((E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }):tail) (E { eid=eid2, labE=lab2, v1=v3, v2=v4, propE=prop2 })
    | eid > eid2 = 
        (E { eid=eid2, labE=lab2, v1=v3, v2=v4, propE=prop2 }):E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }:tail
    | otherwise = 
        E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }:(insertEdges tail (E { eid=eid2, labE=lab2, v1=v3, v2=v4, propE=prop2 }))

--Returns the id of the vertex
returnVid :: V -> String
returnVid V{ vid=vid, lab=_, propV=_ } = vid

--Returns true if the vertex is already in the graph
isAlreadyAddedV :: [V] -> String -> Bool
isAlreadyAddedV [] _ = False
isAlreadyAddedV (V { vid=vid, lab=lab, propV=propV, adjacency=adjacency }:vertices) id
    | id == vid = True
    | otherwise = isAlreadyAddedV vertices id





---------------ADD VALUES TO PROPERTIES----------------

--Adds properties to vertices or edges deppending on the name given
addValuesProp :: PG -> String -> Prop -> String -> PG
addValuesProp PG { vertices=vertices, edges=edges } id prop val 
    | isVertex vertices id = PG { vertices = addVProp vertices id prop val, edges=edges }
    | otherwise = PG { vertices=vertices, edges = addEProp edges id prop val } 

--Adds a property to the vertex given in the input
addVProp :: [V] -> String -> Prop -> String -> [V]
addVProp [] _ _ _ = []
addVProp ((V { vid=vid, lab=lab, propV=propV, adjacency=adj }):vertices) id prop value 
    | vid == id = (V { vid=vid, lab=lab, propV=changeProperties propV prop value, adjacency=adj }):vertices
    | otherwise = [V { vid=vid, lab=lab, propV=propV, adjacency=adj }] ++ (addVProp vertices id prop value)

--Changes the property given by the input of the array of sigmas
changeProperties :: [Sigma] -> Prop -> String -> [Sigma]  
changeProperties [] _ _ = []
changeProperties (Sigma { prop=prop, val=val }:sigmas) propChange valChange 
    | prop == propChange = (Sigma { prop = prop , val = changeValue val valChange }):sigmas
    | otherwise = [Sigma { prop=prop, val=val }] ++ changeProperties sigmas propChange valChange 

--Chaanges the value deppending on the value type
changeValue :: Val -> String -> Val
changeValue (I _) val = I (Just (read val :: Int))
changeValue (D _) val = D (Just (read val :: Double))
changeValue (S _) val = S (Just (read $ show val :: String))
changeValue (B _) val = B (Just (read val :: Bool))

--Adds a property to the edge given by the input
addEProp :: [E] -> String -> Prop -> String -> [E]
addEProp [] _ _ _ = []
addEProp ((E { eid=eid, labE=lab, v1=v1, v2=v2, propE=propE }):edges) id prop value 
    | eid == id = (E { eid=eid, labE=lab, v1=v1, v2=v2, propE=changeProperties propE prop value }):edges
    | otherwise = [E { eid=eid, labE=lab, v1=v1, v2=v2, propE=propE }] ++ (addEProp edges id prop value)





---------------ADDS LABELS FROM INPUT FILES-------------

--Adds a label to a vertex or an edge deppending on the input id
addLabels :: PG -> String -> Lab -> PG 
addLabels (PG { vertices=vertices, edges=edges }) id label 
    | isVertex vertices id = fromJust $ defVlabel (PG { vertices=vertices, edges=edges }) id label  
    | otherwise = fromJust $ defElabel (PG { vertices=vertices, edges=edges }) id label  

--Add label to vertex or return nothing if it already had a label 
defVlabel :: PG -> String -> Lab -> Maybe PG  
defVlabel  (PG { vertices=vertices, edges=edges }) vid label
    | not (vHasLabel vertices vid) = Just PG { vertices=(addVLabel vertices vid label), edges=edges }
    | otherwise = Nothing

--Check if the vertex already had a label 
vHasLabel :: [V] -> String -> Bool
vHasLabel [] _ = False
vHasLabel ((V { vid=vid, lab=lab, propV=_ }):vs) id = (id == vid && lab/="") || vHasLabel vs id

--Add label to vertex 
addVLabel :: [V] -> String -> Lab -> [V]
addVLabel [] _ _ = []
addVLabel ((V { vid=vid, lab=lab, propV=propV, adjacency=adj }):vertices) id label
    | vid == id = (V { vid=vid, lab=label, propV=propV, adjacency=adj }):vertices
    | otherwise = [V { vid=vid, lab=lab, propV=propV, adjacency=adj }] ++ (addVLabel vertices id label)

--Function to know if the label we want to add is to a vertex or to an edge
isVertex :: [V] -> String -> Bool
isVertex [] _ = False
isVertex ((V { vid=vid, lab=_, propV=_ }):vs) id = vid==id || isVertex vs id

--Add label to edge or return nothing
defElabel :: PG -> String -> Lab -> Maybe PG  
defElabel  (PG { vertices=vertices, edges=edges }) eid label
    | not (eHasLabel edges eid) = Just PG { vertices=vertices, edges=addELabel edges eid label }
    | otherwise = Nothing

--Add label to an edge
addELabel :: [E] -> String -> Lab -> [E]
addELabel [] _ _ = []
addELabel ((E { eid=eid, labE=lab, v1=v1, v2=v2, propE=prop }):edges) id label
    | eid == id = (E { eid=eid, labE=label, v1=v1, v2=v2, propE=prop }):edges
    | otherwise = [E { eid=eid, labE=lab, v1=v1, v2=v2, propE=prop }] ++ (addELabel edges id label)

--Check if the edge already had a label
eHasLabel :: [E] -> String -> Bool
eHasLabel [] _ = False
eHasLabel (E { eid=eid, labE=lab, v1=_, v2=_, propE=_ }:es) id = (id == eid && lab/="") || eHasLabel es id





----------------SHOW FUNCTIONS-----------------

--Show the vertices 
showVertices :: [V] -> [String] 
showVertices [] = [] 
showVertices ((V { vid=vid, lab=lab, propV=propV, adjacency=adj }):v) = 
    [vid ++ "[" ++ lab ++ "]" ++ "{" ++ showProperties propV True ++ "}\n"] ++ showVertices v

--Returns true if the value given is nothing
propIsNothing :: Val -> Bool
propIsNothing (I Nothing) = True
propIsNothing (D Nothing) = True
propIsNothing (S Nothing) = True
propIsNothing (B Nothing) = True
propIsNothing _ = False

--Shows value from the maybe type
showValue :: Val -> String
showValue (I val) = show $ fromJust $ val 
showValue (D val) = show $ fromJust $ val 
showValue (S val) = show $ fromJust $ val  
showValue (B val) = show $ fromJust $ val 

--Shows properties from the array of sigmas given 
showProperties :: [Sigma] -> Bool -> String
showProperties [] _ = []
showProperties ((Sigma { prop=prop, val=val }):sigmas) True 
    | not (propIsNothing val) = "(" ++ prop ++ "," ++ showValue val ++ ")" ++ showProperties sigmas False
    | otherwise = showProperties sigmas True
showProperties ((Sigma { prop=prop, val=val }):sigmas) False 
    | not (propIsNothing val) = ",(" ++ prop ++ "," ++ showValue val ++ ")" ++ showProperties sigmas False 
    | otherwise = showProperties sigmas False

--Show edges
showEdges:: [E] -> [String]
showEdges [] = []
showEdges ((E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }):e) = 
    ["(" ++ v1 ++ ")" ++ " - " ++ eid ++ "[" ++ labE ++ "]" ++ "-> " ++ "(" ++ v2 ++ ")" ++ "{" ++ 
    showProperties propE True  ++ "}\n"] ++ showEdges e

--Shows both vertices and edges of the graph
showGraph :: PG -> IO ()
showGraph (PG { vertices=v, edges=e }) = do 
        putStrLn $ flatten $ showVertices v 
        putStrLn $ flatten $ showEdges e 






----------------QUERYING-----------------

---------Sigma query

--Returns an array of sigmas of the vertex or edge given 
sigmaQuery :: PG -> String -> [Sigma]
sigmaQuery PG { vertices=vertices, edges=edges } id
    | isVertex vertices id = getProperties $ fromJust $ returnVertexProperties vertices id
    | otherwise = getProperties $ fromJust $ returnEdgeProperties edges id

--Given a vertex returns its properties
--  if the vertex does not exist returns nothing
returnVertexProperties :: [V] -> String -> Maybe [Sigma]
returnVertexProperties [] _ = Nothing
returnVertexProperties ((V { vid=vid, lab=lab, propV=propV }):vertices) id
    | vid == id = Just propV
    | otherwise = returnVertexProperties vertices id

--Given an edge returns its properties
--  if the edge does not exist returns nothing
returnEdgeProperties :: [E] -> String -> Maybe [Sigma]
returnEdgeProperties [] _ = Nothing
returnEdgeProperties (E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }:edges) id
    | eid == id = Just propE
    | otherwise = returnEdgeProperties edges id

--Returns all the properties with a value 
getProperties :: [Sigma] -> [Sigma]
getProperties [] = []
getProperties (Sigma { prop=prop, val=val }:sigmas) 
    | not (propIsNothing val) = [Sigma { prop=prop, val=val }] ++ getProperties sigmas
    | otherwise = getProperties sigmas



---------PropV query

--Given a number k and a property returns the first k vertices that have a value defined in that property
propVQuery :: PG -> Int -> Prop -> [String]
propVQuery PG { vertices=vertices, edges=edges } k prop = findKVerticesProp vertices k prop

--Finds the k vertices with the property defined
findKVerticesProp :: [V] -> Int -> Prop -> [String]
findKVerticesProp _ 0 _ = []
findKVerticesProp [] _ _ = []
findKVerticesProp (V { vid=vid, lab=lab, propV=propV }:vertices) k prop
    | propIsDefined propV prop = [lab ++ " " ++ returnValue propV prop ++ "\n"] ++ findKVerticesProp vertices (k-1) prop
    | otherwise = findKVerticesProp vertices k prop

--Returns true if the property given is not Nothing
propIsDefined :: [Sigma] -> Prop -> Bool
propIsDefined [] _ = False
propIsDefined (Sigma { prop=prop, val=val }:sigmas) propToFind
    | prop == propToFind && not (propIsNothing val) = True
    | otherwise = propIsDefined sigmas propToFind

--Returns a property value as an String
returnValue :: [Sigma] -> Prop -> String
returnValue [] _ = []
returnValue (Sigma { prop=prop, val=val }:sigmas) propFound
    | prop == propFound = showValue val
    | otherwise = returnValue sigmas propFound



-----------PropE query

--Given a number k and a property returns the first k edges that have a value defined in that property
propEQuery :: PG -> Int -> Prop -> [String]
propEQuery PG { vertices=vertices, edges=edges } k prop = findKEdgesProp edges k prop

--Finds the k edges with the property defined
findKEdgesProp :: [E] -> Int -> Prop -> [String]
findKEdgesProp _ 0 _ = []
findKEdgesProp [] _ _ = []
findKEdgesProp (E { eid=eid, labE=labE, v1=v1, v2=v2, propE=propE }:edges) k prop
    | propIsDefined propE prop = [labE ++ " " ++ returnValue propE prop ++ "\n"] ++ findKEdgesProp edges (k-1) prop
    | otherwise = findKEdgesProp edges k prop



------------kHops query 

--Given a number k, a vertex, a property and a value
--  Returns all the vertices with a k-path from the vertex and the value of their property
kHops :: PG -> [V] -> Int -> V -> Prop -> Val -> [String]
kHops PG { vertices=[], edges=_ } _ _ _ _ _ = []
kHops PG { vertices=(v:vertices), edges=edges } vs k vertex prop val 
    | diffVertexs && reachable && f val valueV && not(propIsNothing valueV)= 
        [vid ++ " " ++ lab ++ " " ++ showValue valueV ++ "\n"] ++ kHops pg vs k vertex prop val
    | otherwise = kHops pg vs k vertex prop val
        where 
            V { vid=id } = vertex
            diffVertexs = returnVid v /= id
            reachable = reachK vs vertex v k
            V { vid=vid, lab=lab, propV=propV } = v
            valueV = getValue propV prop
            pg = PG { vertices=vertices, edges=edges }

--Given an array of sigmas and a property returns the value of the property
getValue :: [Sigma] -> Prop -> Val
getValue (Sigma { prop=prop, val=val }:sigmas) propVal
    | propVal==prop = val
    | otherwise = getValue sigmas propVal

--Function to implement for kHops
f :: Val -> Val -> Bool
f _ _ = True

--BFS that traverses k levels of the graph and returns true if the vertex is at k distance
bfsK :: [V] -> [String] -> String -> Int -> Bool
bfsK _ [] _ _ = False
bfsK vertices (v:vs) vid 0 
    | v == vid = True
    | otherwise = False
bfsK vertices (v:vs) vid k = bfsK vertices vs vid k || bfsK vertices (returnAdjacencies vertices v) vid (k-1) 
            
--Returns the vector of adjacencies of the vertex given
returnAdjacencies :: [V] -> String -> [String]
returnAdjacencies [] _ = []
returnAdjacencies (V {vid=vid, adjacency=adj}:vertices) id
    | vid == id = adj
    | otherwise = returnAdjacencies vertices id

--Returns true if there is a k path from vertex1 to vertex2
reachK :: [V] -> V -> V -> Int -> Bool
reachK vertices V { vid=vid, adjacency=adj } V { vid=vid2 } k = bfsK vertices adj vid2 k

--Returns the vertex with the id given by the input
returnVertex :: [V] -> String -> V
returnVertex [] id = V {vid=id, lab="", propV=[], adjacency=[]}
returnVertex (V { vid=vid, lab=lab, propV=propV, adjacency=adjacency }:vertices) id
    | id == vid = V { vid=vid, lab=lab, propV=propV, adjacency=adjacency }
    | otherwise = returnVertex vertices id



-------reachable

--Returns true if you can go from vertex1 to vertex2 through edges with same label
reachable :: PG -> V -> V -> Lab -> Bool
reachable PG { vertices=vertices, edges=edges } V { vid=vid1, adjacency=adj } V { vid=vid2 } label 
    = bfs vertices edges (returnAdjacenciesWLabel edges adj vid1 label) vid2 label 20

--Traverses the graph until it finds the vertex given or until a certain maximum number
bfs :: [V] -> [E] -> [String] -> String -> String -> Int -> Bool
bfs _ _ [] _ _ _ = False
bfs _ _ _ _ _ 0 = False
bfs vertices edges (v:vs) vidF label k
    | v == vidF = True
    | otherwise = bfs vertices edges vs vidF label (k-1) || bfs vertices edges adjacenciesWLabel vidF label (k-1)
        where
            adjacencies = returnAdjacencies vertices v
            adjacenciesWLabel = returnAdjacenciesWLabel edges adjacencies v label 

--Returns the adjacencies to the vertex with a certain label in the edge
returnAdjacenciesWLabel :: [E] -> [String] -> String -> Lab -> [String]
returnAdjacenciesWLabel _ [] _ _ = []
returnAdjacenciesWLabel edges (adj:adjs) v1 label
    | checkEdgeLabel edges v1 adj label = [adj] ++ returnAdjacenciesWLabel edges adjs v1 label
    | otherwise = returnAdjacenciesWLabel edges adjs v1 label

--Checks if the two vertices have an edge in common with a certain label
checkEdgeLabel :: [E] -> String -> String -> Lab -> Bool
checkEdgeLabel [] _ _ _ = False
checkEdgeLabel (E { v1=v1, v2=v2, labE=labE }:edges) vid1 vid2 label
    | (cond1 || cond2) && cond3 = True 
    | otherwise = checkEdgeLabel edges vid1 vid2 label
        where 
            cond1 = (v1 == vid1 && v2 == vid2) 
            cond2 = (v1 == vid2 && v2 == vid1) 
            cond3 = label == labE





--------------MAIN AND MAIN FUNCTIONS -------------

main :: IO ()
main = do  
    putStrLn $ "Write the four file names in a single line (Ex: rhoFile.pg lambdaFile.pg propFile.pg sigmaFile.pg):"
    contents <- getLine
    let (rho:lambda:prop:sigma:_) = words contents 
    rhoContents <- readFile rho
    lambdaContents <- readFile lambda
    propContents <- readFile prop
    sigmaContents <- readFile sigma
    let graphSigma = populate rhoContents lambdaContents propContents sigmaContents
    showGraph graphSigma

    script graphSigma propContents
    return()

--Populates the graph with the files given 
populate :: String -> String -> String -> String -> PG
populate rhoContents lambdaContents propContents sigmaContents = do
        let propConts = lines $ propContents
        let pgraph = create
        let graphRho = foldl (processRho propConts) pgraph $ lines rhoContents
        let graphLambda = foldl (processLambda) graphRho $ lines lambdaContents
        let graphSigma = foldl (processSigma) graphLambda $ lines sigmaContents
        graphSigma

--Infinite script until exit
script :: PG -> String -> IO ()
script pg propContents = do 
    putStrLn $ "\nWrite any function to apply to the property graph:\n"
    putStrLn $ "    Options: (write the name of the function and the parameters separated by spaces)" ++ 
        "\n" ++ "        sigma(vertex or edge)" ++ "\n" ++
        "        propV(k, Prop)" ++ "\n" ++ "        propE(k, Prop)" ++ "\n" ++ 
        "        kHops(k, vertex, Prop, Type(of the value), Value)"
        ++ "\n" ++ "        reachable(vertex1, vertex2, Label)" ++ "\n"
    putStrLn $ "        addEdge(edge, vertex1, vertex2)" ++ "\n" ++
        "        defVprop(vertex, prop, value)" ++ "\n" ++ "        defEprop(edge, prop, value)" ++ 
        "\n" ++ "        defVlabel(vertex, label)"
        ++ "\n" ++ "        defElabel(edge, label)" ++ "\n\n" ++ "        exit" ++ "\n"
    line <- getLine
    if line /= "exit" then do
        processQuery line pg propContents
        script pg propContents
    else 
        return()

--Given a command it returns the result
processQuery :: String -> PG -> String -> IO ()
processQuery line pg propContents = do
    case (head $ words line) of
        "sigma" -> do 
            let (_:par1:_) = words line
            putStrLn $ showProperties (sigmaQuery pg par1) True
            return()
        "propV" -> do
            let (_:par1:par2:_) = words line 
            let parInt = read par1 :: Int
            putStrLn $ flatten $ propVQuery pg parInt par2
            return()
        "propE" -> do
            let (_:par1:par2:_) = words line 
            let parInt = read par1 :: Int
            putStrLn $ flatten $ propEQuery pg parInt par2
            return()
        "kHops" -> do    
            let (PG { vertices=vertices, edges=edges }) = pg
            let (_:par1:par2:par3:par4:par5:_) = words line
            let val = identifyValue par4 par5 
            let k = read par1 :: Int 
            let v = returnVertex vertices par2
            putStrLn $ flatten $ kHops pg vertices k v par3 val
            return() 
        "reachable" -> do
            let (_:par1:par2:par3:_) = words line 
            let (PG { vertices=vertices, edges=edges }) = pg
            let v2 = returnVertex vertices par2
            let v1 = returnVertex vertices par1
            putStrLn $ show $ reachable pg v1 v2 par3 
        "addEdge" -> do
            let PG { vertices=vertices, edges=edges } = pg
            let (_:par1:par2:par3:_) = words line
            let propConts = lines $ propContents
            let v1 = returnVertex vertices par2 
            let v2 = returnVertex vertices par3 
            let pg2 = addEdge pg par1 v1 v2 propConts
            showGraph pg2
            script pg2 propContents
            return()
        "defVprop" -> do
            let PG { vertices=vertices, edges=edges } = pg
            let (_:par1:par2:par3:_) = words line
            let v2 = addVProp vertices par1 par2 par3
            let pg2 = PG { vertices=v2, edges=edges }
            showGraph pg2
            script pg2 propContents
            return()
        "defEprop" -> do
            let PG { vertices=vertices, edges=edges } = pg
            let (_:par1:par2:par3:_) = words line
            let e2 = addEProp edges par1 par2 par3
            let pg2 = PG { vertices=vertices, edges=e2 }
            showGraph pg2
            script pg2 propContents
            return()
        "defVlabel" -> do
            let (_:par1:par2:_) = words line
            case (defVlabel pg par1 par2) of
                Nothing -> putStrLn "Error: the vertex already has a label"
                Just pg2 -> do
                    showGraph pg2
                    script pg2 propContents
            return()
        "defElabel" -> do
            let (_:par1:par2:_) = words line
            case (defElabel pg par1 par2) of
                Nothing -> putStrLn "Error: the edge already has a label"
                Just pg2 -> do
                    showGraph pg2
                    script pg2 propContents
            return()
        "showGraph" -> do
            showGraph pg
            return()
    return()

--Identifies the type of the value
identifyValue :: String -> String -> Val
identifyValue t val
    | t == "Int" = I (Just (read val :: Int))
    | t == "Double" = D (Just (read val :: Double))
    | t == "String" = S (Just (read $ show val :: String))
    | t == "Bool" = B (Just (read val :: Bool))

--Adds the values of the file sigma
processSigma :: PG -> String -> PG
processSigma pg line = do
    let edgeVertex = first
    let prop = second 
    let value = third 
    addValuesProp pg edgeVertex prop value
    where 
        (first:second:third:_) = words line

--Adds the values of the file rho
processRho :: [String] -> PG -> String -> PG
processRho propConts pg line = do 
    let (PG { vertices=vertices, edges=edges }) = pg
    let v1 = returnVertex vertices second 
    let v2 = returnVertex vertices third 
    addEdge pg first v1 v2 propConts  
    where 
        (first:second:third:_) = words line
        
--Adds the values of the file lambda
processLambda :: PG -> String -> PG
processLambda pg line = do 
    let edgeVertex = first 
    let label = second 
    addLabels pg edgeVertex label 
    where 
        (first:second:_) = words line

--Flatten function
flatten :: [String] -> String 
flatten [] = []
flatten [fin] = fin
flatten (head:tail) = head ++ flatten (tail)

