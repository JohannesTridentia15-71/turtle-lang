{
    module Lexer where
}

%wrapper "basic"

-- preambles
$digit = 0-9
$alpha = a-zA-Z
$hexdigit = $digit | A-F | a-f
$filename = [a-z][a-zA-Z0-9_]*\.ttl -- matches a filename that starts with a lowercase letter, followed by any combination of letters, digits, or underscores, and ends with the .ttl extension
$operands = + | - | * | / | == | < | > | != | <= | >=
$element = "@subject" | "@predicate" | "@object"
$subject = $uri
$predicate = $uri
$object = $uri | [a-zA-Z0-9_]+ -- matches a string that consists of letters, digits, or underscores
$branch = "<"$subject"><"$predicate"><"$object">" -- matches a branch in the form of <subject><predicate><object>

-- URI based on the RFC 3986 Spefication. Taken from lilliemarck at "https://gist.github.com/lilliemarck/4515664.js"
$uri = [A-Za-z][A-Za-z0-9+.-]*:(//(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|:)*@)?([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=])*(:[0-9]*)?(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*|/(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])+(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*)?|([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])+(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*|())(\?(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])|[/?])*)?(#(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])|[/?])*)?|(//(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|:)*@)?([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=])*(:[0-9]*)?(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*|/(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])+(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*)?|([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|@)+(/([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])*)*|())(\?(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])|[/?])*)?(#(([A-Za-z0-9._~-]|%[0-9A-Fa-f]{2}|[!$&'()*+,;=]|[:@])|[/?])*)?

tokens :-
    $white+                 ; 
    "select from"           { \s -> TokenSelectFrom }
    "where"                 { \s -> TokenWhere }
    "select @object"        { \s -> TokenSelectObject }
    "from"                  { \s -> TokenFrom }
    "combine"               { \s -> TokenCombine }
    "add"                   { \s -> TokenAdd }
    "to"                    { \s -> TokenTo }
    "in"                    { \s -> TokenIn }
    "replace"               { \s -> TokenReplace }
    "with"                  { \s -> TokenWith }
    "construct"             { \s -> TokenConstruct }
    $branch                  { \s -> TokenBranch s }
    $filename                { \s -> TokenGraphName s }
    $element                 { \s -> TokenElement s }
    $subject                 { \s -> TokenSubject s }
    $predicate               { \s -> TokenPredicate s }
    $object                  { \s -> TokenObject s }
    $operands                { \s -> TokenOperator s }

{
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
            | TokenBranch
            -- tokens that take arguments
            | TokenGraphName String
            | TokenElement String
            | TokenSubject String
            | TokenPredicate String
            | TokenObject String
            | TokenOperator String
              deriving (Show, Eq)
}