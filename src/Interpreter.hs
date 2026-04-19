module Interpreter
    ( main
    , GraphState
    , evaluate
    , evalQuery
    , evalSelect
    , evalCombine
    , evalFilterStart
    , parseTurtleFile
    , serializeGraph
    , splitTokens
    ) where

import Lexer
import Parser
import Normalise (normaliseTTL)
import System.Environment (getArgs)
import qualified Data.Map as Map -- used for findWithDefault to avoid errors 
import Data.List (isPrefixOf)
import Data.List (isSuffixOf)
import Data.List (nub)
import Data.Char (isDigit)
import Data.List (groupBy, sortBy, maximumBy, minimumBy)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Debug.Trace (traceShow)

type GraphState = Map.Map String [(String, String, String)]

-- Currently this code contains everything - over the next few days I will split into separate files to improve codestyle and readability
-- Correct as of 19/04/2025 - mag1g24

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            input <- readFile file
            let tokens = alexScanTokens input
            let ast = parseTTL tokens

            let ttlFiles = [s | TokenFileName _ s <- tokens] ++ [s | TokenLiteral _ s <- tokens, ".ttl" `isSuffixOf` s]

            initialState <- do
                let uniqueFiles = nub ttlFiles
                fileContents <- mapM (\f -> do
                    content <- readFile f
                    let clean = unlines (normaliseTTL content)
                    return (f, parseTurtleFile clean)
                    ) uniqueFiles
                return $ Map.fromList fileContents
            _ <- evaluate ast initialState
            return ()
        _ -> putStrLn "Usage: stack exec turtle-lang-exe -- <filename>"

-- entry point: evaluate all statements from here
evaluate :: Line -> GraphState -> IO GraphState
evaluate (LSaveQuery query targetName) state = do
    let newTriples = evalQuery query state
    let newState = Map.insert targetName newTriples state
    writeFile targetName (serializeGraph newTriples)
    return newState

evaluate (LNoSaveQuery query) state = do
    let result = evalQuery query state
    putStr (serializeGraph result)
    return state

evaluate (LEval (OpJoin g1 g2 cond) fname) state = do
    let result = evalJoin g1 g2 cond state
    writeFile fname (serializeGraph result)
    return (Map.insert fname result state)

evaluate (LEval operation fname) state = do
    let result = evalOperation operation state
    writeFile fname result
    return state

evaluate (LNoSaveEval (OpJoin g1 g2 cond)) state = do
    let result = evalJoin g1 g2 cond state
    putStr (serializeGraph result)
    return state

evaluate (LNoSaveEval operation) state = do
    let result = evalOperation operation state
    putStrLn result
    return state


evalMax :: String -> GraphState -> [(String, String, String)]
evalMax fileName state =
    let triples = Map.findWithDefault [] fileName state
        sorted = sortBy (comparing (\(s, p, _) -> (s, p))) triples
        groups = groupBy (\(s1, p1, _) (s2, p2, _) -> s1 == s2 && p1 == p2) sorted
        
    in map findMaxInGroup groups

findMaxInGroup :: [(String, String, String)] -> (String, String, String)
findMaxInGroup = maximumBy (comparing (extractNumeric . (\(_, _, o) -> o)))
  where
    extractNumeric s = 
        let cleaned = filter (\c -> isDigit c || c == '.' || c == '-') s
        in case readMaybe cleaned :: Maybe Double of
             Just n  -> n
             Nothing -> 0.0 

evalMin :: String -> GraphState -> [(String, String, String)]
evalMin fileName state =
    let triples = Map.findWithDefault [] fileName state
        sorted = sortBy (comparing (\(s, p, _) -> (s, p))) triples
        groups = groupBy (\(s1, p1, _) (s2, p2, _) -> s1 == s2 && p1 == p2) sorted
        
    in map findMinInGroup groups

findMinInGroup :: [(String, String, String)] -> (String, String, String)
findMinInGroup = minimumBy (comparing (extractNumeric . (\(_, _, o) -> o)))
  where
    extractNumeric s = 
        let cleaned = filter (\c -> isDigit c || c == '.' || c == '-') s
        in case readMaybe cleaned :: Maybe Double of
             Just n  -> n
             Nothing -> 0.0 


evalQuery :: Query -> GraphState -> [(String, String, String)]
evalQuery (QDelete dq)    state = evalDelete dq state
evalQuery (QAdd aq)       state = evalAdd aq state
evalQuery (QCombine cq)   state = evalCombine cq state
evalQuery (QReplace rq)   state = evalReplace rq state
evalQuery (QConstruct cq) state = evalConstruct cq state

-- Delete Case
evalDelete :: DeleteQuery -> GraphState -> [(String, String, String)]
evalDelete (Dq name) state = [] 
evalDelete (DqWhere name fStart) state =
    let g = Map.findWithDefault [] name state
    in [t | t <- g, not (evalFilterStart fStart t state)]

-- Add Case
evalAdd :: AddQuery -> GraphState -> [(String, String, String)]
evalAdd (AddQ branchStr name) state =
    let g = Map.findWithDefault [] name state
        clean = tail (init branchStr) 
        tokens = splitTokens clean
    in if length tokens >= 3 
       then (tokens !! 0, tokens !! 1, tokens !! 2) : g
       else g

-- Combine Case
evalCombine :: CombineQuery -> GraphState -> [(String, String, String)]
evalCombine (CNested sq) state = evalSelect sq state
evalCombine (CCombine cq sq) state = 
    nub (evalCombine cq state ++ evalSelect sq state)

-- Replace Case
evalReplace :: ReplaceQuery -> GraphState -> [(String, String, String)]
evalReplace (RqObject targetName selectQuery newElem) state =
    let g = Map.findWithDefault [] targetName state
        -- Identify which triples match the selection to be replaced
        toReplace = evalSelectEmpty selectQuery state
    in map (\t -> if t `elem` toReplace then updateTriple t newElem else t) g


-- Construct Case 
evalConstruct :: ConstructQuery -> GraphState -> [(String, String, String)]
evalConstruct (Cq name) _ = [] -- Usually creates a new empty graph structure


-- select statements
evalSelect :: SelectQuery -> GraphState -> [(String, String, String)]
evalSelect (SQAll name) state = 
    Map.findWithDefault [] name state
evalSelect (SQWhere name fStart) state =
    let g = Map.findWithDefault [] name state
    in [t | t <- g, evalFilterStart fStart t state]
evalSelect (SQElement (SElem elem name fStart)) state =
    let g = Map.findWithDefault [] name state
    in [ (extract elem t, "", "") | t <- g, evalFilterStart fStart t state ]

-- 
evalOperation :: Operation -> GraphState -> String
evalOperation (OpArith (Arith op sel1 sel2)) state = 
    let v1 = read (evalSelectElement sel1 ("", "", "") state) :: Int
        v2 = read (evalSelectElement sel2 ("", "", "") state) :: Int
    in case op of
        "+" -> show (v1 + v2)
        "-" -> show (v1 - v2)
        "*" -> show (v1 * v2)
        _   -> "0"

evalOperation (OpComp comp) state = 
    if evalComparison comp ("", "", "") state then "True" else "False"

evalOperation (OpGraph gOp) state = evalGraphOp gOp state
evalOperation (OpJoin g1 g2 cond) state = serializeGraph (evalJoin g1 g2 cond state)

-- Transitive Join Logic (only works for #type and #subClassOf - might need to be changed?)
evalJoin :: String -> String -> (Element, String, Element) -> GraphState -> [(String, String, String)]
evalJoin g1Name g2Name (elem1, op, elem2) state =
    let 
        g1 = Map.findWithDefault [] g1Name state
        g2 = Map.findWithDefault [] g2Name state

        rdfType = "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
        rdfsSub = "<http://www.w3.org/2000/01/rdf-schema#subClassOf>"

    -- join condition 
    in nub [ (s1, rdfType, o2) 
           | (s1, p1, o1) <- g1, p1 == rdfType
           , (s2, p2, o2) <- g2, p2 == rdfsSub
           , o1 == s2 ] 


-- Graph Operation Logic
evalGraphOp :: GraphOperation -> GraphState -> String
evalGraphOp op state = case op of
    GUnion sel1 sel2 -> 
        serializeGraph $ nub (getGraphBySel sel1 state ++ getGraphBySel sel2 state)

    GIntersection sel1 sel2 ->
        let g1 = getGraphBySel sel1 state
            g2 = getGraphBySel sel2 state
        in serializeGraph [t | t <- g1, t `elem` g2]

    GDifference sel1 sel2 ->
        let g1 = getGraphBySel sel1 state
            g2 = getGraphBySel sel2 state
        in serializeGraph [t | t <- g1, not (t `elem` g2)]

    GSingle (GMax (SQAll f)) -> serializeGraph (evalMax f state)
    
    GSingle (GMin (SQAll f)) -> serializeGraph (evalMin f state)
 
    GSingle (GMax (SQElement sel)) -> 
        let values = getAllValues sel state
        in if null values then "Empty" else maximum values
    
    _ -> "Operation Result"

-- Helper for min/max - get all values 
getAllValues :: SelectElementQuery -> GraphState -> [String]
getAllValues (SElem elem target fStart) state =
    let g = Map.findWithDefault [] target state
    in [extract elem t | t <- g, evalFilterStart fStart t state]
getAllValues _ _ = []

getGraphBySel :: SelectQuery -> GraphState -> [(String, String, String)]
getGraphBySel (SQAll name) state = Map.findWithDefault [] name state
getGraphBySel (SQWhere name fStart) state = 
    let g = Map.findWithDefault [] name state
    in [t | t <- g, evalFilterStart fStart t state]
getGraphBySel _ _ = []


-- Select an element 
evalSelectElement :: SelectElementQuery -> (String, String, String) -> GraphState -> String
evalSelectElement (SELit lit) _ _ = lit
evalSelectElement (SElemSimple elem) triple _ = extract elem triple
evalSelectElement (SElem elem target fStart) _ state =
    let g = Map.findWithDefault [] target state
    in case [extract elem t | t <- g, evalFilterStart fStart t state] of
        (val:_) -> val
        []      -> ""

-- Helper
extract :: Element -> (String, String, String) -> String
extract ESubject   (s, _, _) = s
extract EPredicate (_, p, _) = p
extract EObject    (_, _, o) = o

-- Comparative operators
evalComparison :: ComparisonOperation -> (String, String, String) -> GraphState -> Bool
evalComparison (Comp op sel1 sel2) triple state =
    let val1 = evalSelectElement sel1 triple state
        val2 = evalSelectElement sel2 triple state
    in if all isDigit val1 && all isDigit val2 && not (null val1 || null val2)
       then case op of
           "=" -> (read val1 :: Int) == (read val2)
           "==" -> (read val1 :: Int) == (read val2)
           ">=" -> (read val1 :: Int) >= (read val2)
           "<=" -> (read val1 :: Int) <= (read val2)
           ">"  -> (read val1 :: Int) >  (read val2)
           "<"  -> (read val1 :: Int) <  (read val2)
           _    -> False
       else case op of
           "=" -> val1 == val2
           "==" -> val1 == val2
           "!=" -> val1 /= val2
           _    -> False

-- Evaluate with filters 
evalFilterStart :: FilterStart -> (String, String, String) -> GraphState -> Bool
evalFilterStart (FAnd f1 f2) t state = evalFilter f1 t state && evalFilterFinal f2 t state
evalFilterStart (FOr f1 f2)  t state = evalFilter f1 t state || evalFilterFinal f2 t state
evalFilterStart (FNot f)     t state = not (evalFilter f t state)
evalFilterStart (FBase f)    t state = evalFilter f t state

evalFilter :: Filter -> (String, String, String) -> GraphState -> Bool
evalFilter (FAnd' f1 f2) t state = evalFilter f1 t state && evalFilterFinal f2 t state
evalFilter (FOr' f1 f2)  t state = evalFilter f1 t state || evalFilterFinal f2 t state
evalFilter (FNot' f)     t state = not (evalFilter f t state)
evalFilter (FComp comp)  t state = evalComparison comp t state

evalFilterFinal :: FilterFinal -> (String, String, String) -> GraphState -> Bool
evalFilterFinal (FFinal comp) t state = evalComparison comp t state

evalSelectEmpty :: SelectEmptyQuery -> GraphState -> [(String, String, String)]
evalSelectEmpty (SENoElemAll name) state = Map.findWithDefault [] name state
evalSelectEmpty (SENoElemWhere name fStart) state =
    let g = Map.findWithDefault [] name state
    in [t | t <- g, evalFilterStart fStart t state]



-- Helper Functions to parse the normalised .ttl file
parseTurtleFile :: String -> [(String, String, String)]
parseTurtleFile content = 
    [ (s, p, o) | line <- lines content,
                  let w = splitTokens line,
                  length w >= 3,
                  let s = w !! 0,
                  let p = w !! 1,
                  let o = w !! 2 ]

splitTokens :: String -> [String]
splitTokens "" = []
splitTokens (x:xs)
    | x == ' '  = splitTokens xs
    | x == '"'  = let (quoted, rest) = break (== '"') xs
                  in ('"' : quoted ++ "\"") : splitTokens (drop 1 rest)
    | otherwise = let (word, rest) = break (\c -> c == ' ' || c == '"') (x:xs)
                  in word : splitTokens rest

serializeGraph :: [(String, String, String)] -> String
serializeGraph triples = unlines [s ++ " " ++ p ++ " " ++ o ++ " ." | (s, p, o) <- triples]

updateTriple :: (String, String, String) -> String -> (String, String, String)
updateTriple (s, p, o) newVal
    | "#subject"   `isPrefixOf` newVal = (newVal, p, o)
    | "#predicate" `isPrefixOf` newVal = (s, newVal, o)
    | otherwise                        = (s, p, newVal)