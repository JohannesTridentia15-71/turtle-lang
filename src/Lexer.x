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
@uri = [A-Za-z0-9$special\:]+
@graphname = [a-z][a-zA-Z0-9\-_]*.ttl
@filename = [a-zA-Z0-9\-_]+\.ttl
-- defined as to avoid repettion in the branch rule
@element = @uri | @literal
@branch = \<@uri\>\<@uri\>\<@element\>
@uri_ref = \<@uri\>
@operands = \+|\*|\-|\\
@comparison = ==|\=|!\=|\<|\>|\<\=|\>\=

tokens :-

    $white+                 ; 
    \(                      ;
    \)                      ;
    "select_from"           { (\p s -> TokenSelectFrom p) }
    "select object"         { (\p s -> TokenSelectObject p) }
    "transitive_join"       { (\p s -> TokenJoin p) }
    "select"                { (\p s -> TokenSelect p) }
    "where"                 { (\p s -> TokenWhere p) }
    "from"                  { (\p s -> TokenFrom p) }
    "combine"               { (\p s -> TokenCombine p) }
    "add"                   { (\p s -> TokenAdd p) }
    "to"                    { (\p s -> TokenTo p) }
    "in"                    { (\p s -> TokenIn p) }
    "replace"               { (\p s -> TokenReplace p) }
    "with"                  { (\p s -> TokenWith p) }
    "construct"             { (\p s -> TokenConstruct p) }
    "delete"                { (\p s -> TokenDelete p) }
    "start"                 { (\p s -> TokenStart p) }
    "evaluate"              { (\p s -> TokenEvaluate p) }
    "union"                 { (\p s -> TokenUnion p) }
    "intersection"          { (\p s -> TokenIntersection p) }
    "difference"            { (\p s -> TokenDifference p) }
    "min"                   { (\p s -> TokenMin p) }
    "max"                   { (\p s -> TokenMax p) }
    "count"                 { (\p s -> TokenCount p) }
    "and"                   { (\p s -> TokenAnd p) }
    "or"                    { (\p s -> TokenOr p) }
    "not"                   { (\p s -> TokenNot p) }
    "#subject"              { (\p s -> TokenSubjectElement p) }
    "#predicate"            { (\p s -> TokenPredicateElement p) }
    "#object"               { (\p s -> TokenObjectElement p) }
    "save_to"               { (\p s -> TokenSaveTo p s) }
    "|"                     { (\p s -> TokenPipe p ) }
    @branch                 { (\p s -> TokenBranch p s) }
    @uri_ref                { (\p s -> TokenURIRef p s) }
    @filename               { (\p s -> TokenFileName p s) }
    @graphname              { (\p s -> TokenGraphName p s) }
    @operands               { (\p s -> TokenArithOperator p s) }
    @comparison             { (\p s -> TokenCompOperator p s) }
    @literal                { (\p s -> TokenLiteral p s) }
    .                       { (\p s -> TtlError p s) } 



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
                | TokenStart AlexPosn
                | TokenEvaluate AlexPosn
                | TokenUnion AlexPosn
                | TokenIntersection AlexPosn
                | TokenDifference AlexPosn
                | TokenMin AlexPosn
                | TokenMax AlexPosn
                | TokenCount AlexPosn
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
                | TokenJoin AlexPosn
                | TokenURIRef AlexPosn String
                | TtlError AlexPosn String
                | TokenPipe AlexPosn
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
tokenPosn (TokenStart (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEvaluate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntersection (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDifference (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)    
tokenPosn (TokenCount (AlexPn a l c)) = show(l) ++ ":" ++ show(c)    
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
tokenPosn (TokenJoin (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenURIRef (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TtlError (AlexPn a l c) s) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPipe (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}