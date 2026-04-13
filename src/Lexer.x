{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@literal = [a-zA-Z0-9\-\"_]+
-- note that this is a very loose, non-binding definition of the uri syntax, but it should be sufficient for our purposes
-- the RFC 3986 definition is much too complicated. 
-- it could accept some invalid URIs, this might be a weakpoint in the future.
@uri = [A-Za-z0-9$special]+
@ttl = \.ttl
@filename = [a-z][a-zA-Z0-9\-_]*@ttl
@graphname = @filename
-- defined as to avoid repettion in the branch rule
@element = @uri | @literal
@branch = \< @uri \>\< @uri \>\< @element \>
@operands = \+|\*|\-|\\
@comparison = \=|!\=|\<|\>|\<\=|\>\=

tokens :-

    $white+                 ; 
    "select"                { (\p s -> TokenSelect p) }
    "select from"           { (\p s -> TokenSelectFrom p) }
    "where"                 { (\p s -> TokenWhere p) }
    "select #object"        { (\p s -> TokenSelectObject p) }
    "from"                  { (\p s -> TokenFrom p) }
    "combine"               { (\p s -> TokenCombine p) }
    "add"                   { (\p s -> TokenAdd p) }
    "to"                    { (\p s -> TokenTo p) }
    "in"                    { (\p s -> TokenIn p) }
    "replace"               { (\p s -> TokenReplace p) }
    "with"                  { (\p s -> TokenWith p) }
    "construct"             { (\p s -> TokenConstruct p) }
    "delete"                { (\p s -> TokenDelete p) }
    "evaluate"              { (\p s -> TokenEvaluate p) }
    "union"                 { (\p s -> TokenUnion p) }
    "intersection"          { (\p s -> TokenIntersection p) }
    "difference"            { (\p s -> TokenDifference p) }
    "min"                   { (\p s -> TokenMin p) }
    "max"                   { (\p s -> TokenMax p) }
    "#subject"              { (\p s -> TokenSubjectElement p) }
    "#predicate"            { (\p s -> TokenPredicateElement p) }
    "#object"               { (\p s -> TokenObjectElement p) }
    "save to"               { (\p s -> TokenSaveTo p (read s)) }
    "and"                   { (\p s -> TokenAnd p) }
    "or"                    { (\p s -> TokenOr p) }
    "not"                   { (\p s -> TokenNot p) }
    @branch                 { (\p s -> TokenBranch p (read s)) }
    @filename               { (\p s -> TokenFileName p (read s)) }
    @graphname              { (\p s -> TokenGraphName p (read s)) }
    @operands               { (\p s -> TokenArithOperator p (read s)) }
    @comparison             { (\p s -> TokenCompOperator p (read s)) }
    @literal                { (\p s -> TokenLiteral p (read s)) }



{

data TtlToken = TokenSelectFrom AlexPosn
                | TokenSelect AlexPosn
                | TokenWhere AlexPosn 
                | TokenSelectObject AlexPosn
                | TokenFrom AlexPosn
                | TokenCombine AlexPosn
                | TokenAdd AlexPosn
                | TokenTo AlexPosn
                | TokenIn AlexPosn
                | TokenAnd AlexPosn
                | TokenOr AlexPosn
                | TokenNot AlexPosn
                | TokenReplace AlexPosn
                | TokenWith AlexPosn
                | TokenConstruct AlexPosn
                | TokenDelete AlexPosn
                | TokenEvaluate AlexPosn
                | TokenUnion AlexPosn
                | TokenIntersection AlexPosn
                | TokenDifference AlexPosn
                | TokenMin AlexPosn
                | TokenMax AlexPosn
                | TokenSubjectElement AlexPosn
                | TokenPredicateElement AlexPosn
                | TokenObjectElement AlexPosn
                | TokenLiteral AlexPosn String
                | TokenGraphName AlexPosn String
                | TokenFileName AlexPosn String
                | TokenBranch AlexPosn String
                | TokenElement AlexPosn String
                | TokenArithOperator AlexPosn String
                | TokenCompOperator AlexPosn String
                | TokenSaveTo AlexPosn String
                deriving (Show, Eq)

--useful for error reporting, to get the position of the token that caused the error
tokenPosn :: TtlToken -> String
tokenPosn (TokenSelectFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSelect (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhere (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSelectObject (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCombine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAdd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReplace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWith (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConstruct (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDelete (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEvaluate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntersection (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDifference (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)    
tokenPosn (TokenMax (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubjectElement (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPredicateElement (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenObjectElement (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLiteral (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGraphName (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFileName (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBranch (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElement (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenArithOperator (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCompOperator (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSaveTo (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
}