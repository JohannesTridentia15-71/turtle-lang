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
@operands = \+\*\-\\

tokens :-

    $white+                 ; 
    "select from"           { \p s -> PT p TokenSelectFrom }
    "where"                 { \p s -> PT p TokenWhere }
    "select @object"        { \p s -> PT p TokenSelectObject }
    "from"                  { \p s -> PT p TokenFrom }
    "combine"               { \p s -> PT p TokenCombine }
    "add"                   { \p s -> PT p TokenAdd }
    "to"                    { \p s -> PT p TokenTo }
    "in"                    { \p s -> PT p TokenIn }
    "replace"               { \p s -> PT p TokenReplace }
    "with"                  { \p s -> PT p TokenWith }
    "construct"             { \p s -> PT p TokenConstruct }
    "delete"                { \p s -> PT p TokenDelete }
    "union"                 { \p s -> PT p TokenUnion }
    "intersection"          { \p s -> PT p TokenIntersection }
    "difference"            { \p s -> PT p TokenDifference }
    "min"                   { \p s -> PT p TokenMin }
    "max"                   { \p s -> PT p TokenMax }
    "#subject"              { \p s -> PT p TokenSubjectElement }
    "#predicate"            { \p s -> PT p TokenPredicateElement  }
    "#object"               { \p s -> PT p TokenObjectElement  }
    @branch                 { \p s -> PT p (TokenBranch s) }
    @filename               { \p s -> PT p (TokenFileName s) }
    @graphname              { \p s -> PT p (TokenGraphName s) }
    @element                { \p s -> PT p (TokenElement s) }
    @operands               { \p s -> PT p (TokenOperator s) }



{
data PosnToken = PT AlexPosn Token deriving (Eq, Show)


data Token = TokenSelectFrom
            | TokenWhere
            | TokenSelectObject
            | TokenFrom
            | TokenCombine
            | TokenAdd
            | TokenTo
            | TokenIn
            | TokenReplace
            | TokenWith 
            | TokenConstruct
            | TokenDelete
            | TokenUnion
            | TokenIntersection
            | TokenDifference
            | TokenMin
            | TokenMax
            | TokenSubjectElement 
            | TokenPredicateElement
            | TokenObjectElement
            | TokenGraphName String
            | TokenFileName String
            | TokenBranch String
            | TokenElement String
            | TokenOperator String
              deriving (Show, Eq)
}