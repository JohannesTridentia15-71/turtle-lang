module Normalise where 
import Data.List (isPrefixOf, isSuffixOf)

normaliseTTL :: String -> [String]
normaliseTTL input =
    let allLines    = lines input
        abbrevLines = filter (\l -> not (null l) && head l == '@') allLines
        triples     = filter (\l -> not (null l) && head l /= '@') allLines
        expanded    = expandLines abbrevLines triples
        joined      = joinContinuations expanded
    in concatMap expandCommas (concatMap expandSemicolons joined)

joinContinuations :: [String] -> [String]
joinContinuations [] = []
joinContinuations (l:ls)
    | endsWithSemi l = case ls of
        []          -> [l]
        (next:rest) -> joinContinuations ((l ++ " " ++ next) : rest)
    | otherwise = l : joinContinuations ls

endsWithSemi :: String -> Bool
endsWithSemi s =
    let toks = tokeniseLine s
    in not (null toks) && last toks == ";"

tokeniseLine :: String -> [String]
tokeniseLine "" = []
tokeniseLine (x:xs)
    | x == ' '  = tokeniseLine xs
    | x == '"'  = let (quoted, rest) = break (== '"') xs
                  in ('"' : quoted ++ "\"") : tokeniseLine (drop 1 rest)
    | otherwise = let (word, rest) = break (\c -> c == ' ' || c == '"') (x:xs)
                  in word : tokeniseLine rest

expandSemicolons :: String -> [String]
expandSemicolons input =
    let toks = tokeniseLine input
    in if ";" `notElem` toks
       then [input]
       else
           let toks' = filter (/= ".") toks
           in case toks' of
               (subj:rest) ->
                   let groups = splitBySemi rest
                   in concatMap (\grp -> case grp of
                       [] -> []
                       _  -> [unwords ([subj] ++ grp ++ ["."])]) groups
               _ -> [input]

splitBySemi :: [String] -> [[String]]
splitBySemi [] = []
splitBySemi xs =
    let (grp, rest) = break (== ";") xs
    in grp : case rest of
        []       -> []
        (_:next) -> splitBySemi next

expandLines :: [String] -> [String] -> [String]
expandLines [] triples = triples
expandLines (a:abb) triples = expandLines abb (expand a triples)


expand :: String -> [String] -> [String]
expand abbrev triples
    | "@base" `isPrefixOf` abbrev   = expandBase (extractUri abbrev) triples
    | "@prefix" `isPrefixOf` abbrev = 
        let ws = words abbrev
            pName = head (drop 1 ws)
            pUri  = extractUri abbrev
        in expandPrefix pName pUri triples
    | otherwise = triples


extractUri :: String -> String
extractUri xs = 
    let start = dropWhile (/= '<') xs
        end   = takeWhile (/= '>') start ++ ">"
    in if null start then "" else end


expandBase :: String -> [String] -> [String]
expandBase _ [] = []
expandBase b (t:ts) = unwords (map (applyBase b) (words t)) : expandBase b ts

applyBase :: String -> String -> String
applyBase b token
    | isRelativeIri token = init b ++ tail token
    | otherwise = token


expandPrefix :: String -> String -> [String] -> [String]
expandPrefix _ _ [] = []
expandPrefix pName pUri (t:ts) = unwords (map (applyPrefix pName pUri) (words t)) : expandPrefix pName pUri ts

applyPrefix :: String -> String -> String -> String
applyPrefix pName pUri token
    | pName `isPrefixOf` token = init pUri ++ drop (length pName) token ++ ">"
    | otherwise = token


isRelativeIri :: String -> Bool
isRelativeIri xs = 
    not (null xs) && 
    head xs == '<' && 
    last xs == '>' && 
    not ("http" `isPrefixOf` drop 1 xs)


expandCommas :: String -> [String]
expandCommas input =
    let
        ws    = words (filter (`notElem` ".;") input)
        rawWs = filter (`notElem` [".", ";"]) (words input)
    in if "," `notElem` ws
       then [input]
       else
           let subj = rawWs !! 0
               pred = rawWs !! 1
               objs = splitByComma (drop 2 rawWs)
           in map (\obj -> unwords [subj, pred, obj, "."]) objs


splitByComma :: [String] -> [String]
splitByComma [] = []
splitByComma xs = 
    let (item, rest) = break (== ",") xs
    in unwords item : case rest of
        [] -> []
        (_:next) -> splitByComma next

-- This main function shouldn't be used, as this file won't be accessed directly
main :: IO ()
main = do
    contents <- readFile "example.ttl"
    let result = normaliseTTL contents
    mapM_ putStrLn result