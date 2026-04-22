module Main (main) where

import Test.HUnit
import qualified Data.Map as Map
import Lexer
import Parser
import Normalise (normaliseTTL)
import Interpreter
    ( GraphState
    , evalSelect
    , evalCombine
    , evalFilterStart
    , parseTurtleFile
    , serializeGraph
    , splitTokens
    )

-- Helpers

parse :: String -> Line
parse input = case parseTTL (alexScanTokens input) of
    LBase l -> l
    LPipedQuery _ _ -> error "parse: expected single line, got pipeline"

tokenTag :: TtlToken -> String
tokenTag (TokenSelectFrom _)    = "SelectFrom"
tokenTag (TokenSelect _)        = "Select"
tokenTag (TokenWhere _)         = "Where"
tokenTag (TokenCombine _)       = "Combine"
tokenTag (TokenAdd _)           = "Add"
tokenTag (TokenTo _)            = "To"
tokenTag (TokenIn _)            = "In"
tokenTag (TokenAnd _)           = "And"
tokenTag (TokenOr _)            = "Or"
tokenTag (TokenNot _)           = "Not"
tokenTag (TokenStart _)         = "Start"
tokenTag (TokenEvaluate _)      = "Evaluate"
tokenTag (TokenUnion _)         = "Union"
tokenTag (TokenMin _)           = "Min"
tokenTag (TokenMax _)           = "Max"
tokenTag (TokenJoin _)          = "Join"
tokenTag (TokenDelete _)        = "Delete"
tokenTag (TokenConstruct _)     = "Construct"
tokenTag (TokenSubjectElement _)   = "Subject"
tokenTag (TokenPredicateElement _) = "Predicate"
tokenTag (TokenObjectElement _)    = "Object"
tokenTag (TokenFileName _ _)    = "FileName"
tokenTag (TokenGraphName _ _)   = "GraphName"
tokenTag (TokenLiteral _ _)     = "Literal"
tokenTag (TokenSaveTo _ _)      = "SaveTo"
tokenTag (TokenCompOperator _ _) = "CompOp"
tokenTag (TokenArithOperator _ _) = "ArithOp"
tokenTag _                      = "Other"

-- Lexer tests

lexerTests :: Test
lexerTests = TestLabel "Lexer" $ TestList
    [ "lex: select from" ~:
        map tokenTag (alexScanTokens "select from foo.ttl")
        ~?= ["SelectFrom", "FileName"]

    , "lex: combine keyword" ~:
        map tokenTag (alexScanTokens "combine")
        ~?= ["Combine"]

    , "lex: where keyword" ~:
        map tokenTag (alexScanTokens "where")
        ~?= ["Where"]

    , "lex: and or not keywords are distinct" ~:
        map tokenTag (alexScanTokens "and or not")
        ~?= ["And", "Or", "Not"]

    , "lex: start keyword" ~:
        map tokenTag (alexScanTokens "start")
        ~?= ["Start"]

    , "lex: evaluate keyword" ~:
        map tokenTag (alexScanTokens "evaluate")
        ~?= ["Evaluate"]

    , "lex: element keywords" ~:
        map tokenTag (alexScanTokens "#subject #predicate #object")
        ~?= ["Subject", "Predicate", "Object"]

    , "lex: save to is a single token" ~:
        map tokenTag (alexScanTokens "save to result.ttl")
        ~?= ["SaveTo", "FileName"]

    , "lex: comparison operators" ~:
        map (\t -> case t of TokenCompOperator _ s -> s; _ -> "?")
            (alexScanTokens ">= <= > < != =")
        ~?= [">=", "<=", ">", "<", "!=", "="]

    , "lex: plain literal" ~:
        map tokenTag (alexScanTokens "hello")
        ~?= ["Literal"]

    , "lex: file name token" ~:
        let ts = alexScanTokens "foo.ttl"
        in case ts of
            [TokenFileName _ s] -> s ~?= "foo.ttl"
            _                   -> "token shape" ~?= "TokenFileName"

    , "lex: transitive_join keyword" ~:
        map tokenTag (alexScanTokens "transitive_join")
        ~?= ["Join"]

    , "lex: delete keyword" ~:
        map tokenTag (alexScanTokens "delete")
        ~?= ["Delete"]

    , "lex: min and max keywords" ~:
        map tokenTag (alexScanTokens "min max")
        ~?= ["Min", "Max"]
    ]

-- Parser tests

parserTests :: Test
parserTests = TestLabel "Parser" $ TestList
    [ "parse: select from produces SQAll" ~:
        parse "select from foo.ttl"
        ~?= LNoSaveQuery (QCombine (CNested (SQAll "foo.ttl")))

    , "parse: combine two graphs" ~:
        parse "combine select from foo.ttl select from bar.ttl"
        ~?= LNoSaveQuery
              (QCombine (CCombine (CNested (SQAll "foo.ttl")) (SQAll "bar.ttl")))

    , "parse: select from with save to" ~:
        parse "select from foo.ttl save to bar.ttl"
        ~?= LSaveQuery (QCombine (CNested (SQAll "foo.ttl"))) "bar.ttl"

    , "parse: delete graph" ~:
        parse "delete foo.ttl"
        ~?= LNoSaveQuery (QDelete (Dq "foo.ttl"))

    , "parse: construct graph" ~:
        parse "construct"
        ~?= LNoSaveQuery QConstruct

    , "parse: add branch to graph" ~:
        parse "add <a><b><c>"
        ~?= LNoSaveQuery (QAdd (AddQ "<a><b><c>"))

    , "parse: simple where filter (no start)" ~:
        parse "select from foo.ttl where = #predicate CompanyA"
        ~?= LNoSaveQuery (QCombine (CNested
              (SQWhere "foo.ttl"
                (FBase (FComp (Comp "=" (SElemSimple EPredicate) (SELit "CompanyA")))))))

    , "parse: compound where filter with start" ~:
        parse "select from foo.ttl where start = #predicate CompanyA and start = #object London"
        ~?= LNoSaveQuery (QCombine (CNested
              (SQWhere "foo.ttl"
                (FAnd
                  (FComp (Comp "=" (SElemSimple EPredicate) (SELit "CompanyA")))
                  (FFinal (Comp "=" (SElemSimple EObject) (SELit "London")))))))

    , "parse: evaluate union" ~:
        parse "evaluate union select from foo.ttl select from bar.ttl"
        ~?= LNoSaveEval (OpGraph (GUnion (SQAll "foo.ttl") (SQAll "bar.ttl")))

    , "parse: evaluate max" ~:
        parse "evaluate max select from foo.ttl"
        ~?= LNoSaveEval (OpGraph (GSingle (GMax (SQAll "foo.ttl"))))

    , "parse: evaluate min" ~:
        parse "evaluate min select from foo.ttl"
        ~?= LNoSaveEval (OpGraph (GSingle (GMin (SQAll "foo.ttl"))))

    , "parse: evaluate intersection" ~:
        parse "evaluate intersection select from foo.ttl select from bar.ttl"
        ~?= LNoSaveEval (OpGraph (GIntersection (SQAll "foo.ttl") (SQAll "bar.ttl")))
    ]

-- Normalise tests

normaliseTests :: Test
normaliseTests = TestLabel "Normalise" $ TestList
    [ "normalise: plain triple is unchanged" ~:
        normaliseTTL "<:s> <:p> <:o> ."
        ~?= ["<:s> <:p> <:o> ."]

    , "normalise: @prefix line is stripped from output" ~:
        length (normaliseTTL "@prefix ex: <http://example.org/> .\nex:Alice ex:age 25 .")
        ~?= 1

    , "normalise: @prefix is expanded in triples" ~:
        normaliseTTL "@prefix ex: <http://example.org/> .\nex:Alice ex:age 25 ."
        ~?= ["<http://example.org/Alice> <http://example.org/age> 25 ."]

    , "normalise: @base expands relative IRIs" ~:
        normaliseTTL "@base <http://example.org/> .\n<Alice> <age> 25 ."
        ~?= ["<http://example.org/Alice> <http://example.org/age> 25 ."]

    , "normalise: comma-separated objects are split" ~:
        normaliseTTL "<:s> <:p> <:o1> , <:o2> ."
        ~?= ["<:s> <:p> <:o1> .", "<:s> <:p> <:o2> ."]

    , "normalise: empty input gives empty output" ~:
        normaliseTTL ""
        ~?= []
    ]

-- Interpreter (pure function) tests

sampleState :: GraphState
sampleState = Map.fromList
    [ ( "people.ttl"
      , [ ("<:Alice>",   "<:age>",      "30")
        , ("<:Bob>",     "<:age>",      "19")
        , ("<:Charlie>", "<:age>",      "21")
        , ("<:Alice>",   "<:worksFor>", "<:CompanyA>")
        ]
      )
    , ( "orgs.ttl"
      , [ ("<:CompanyA>", "<:locatedIn>", "London") ]
      )
    ]

interpreterTests :: Test
interpreterTests = TestLabel "Interpreter" $ TestList
    [ "splitTokens: splits on spaces" ~:
        splitTokens "<:s> <:p> <:o>"
        ~?= ["<:s>", "<:p>", "<:o>"]

    , "splitTokens: keeps quoted strings whole" ~:
        splitTokens "<:s> <:p> \"hello world\""
        ~?= ["<:s>", "<:p>", "\"hello world\""]

    , "parseTurtleFile: parses three triples" ~:
        parseTurtleFile "<:a> <:b> <:c> .\n<:d> <:e> <:f> .\n<:g> <:h> <:i> ."
        ~?= [ ("<:a>", "<:b>", "<:c>")
            , ("<:d>", "<:e>", "<:f>")
            , ("<:g>", "<:h>", "<:i>")
            ]

    , "parseTurtleFile: blank lines are ignored" ~:
        parseTurtleFile "\n<:s> <:p> <:o> .\n"
        ~?= [("<:s>", "<:p>", "<:o>")]

    , "serializeGraph: formats triples with dot suffix" ~:
        serializeGraph [("<:s>", "<:p>", "<:o>")]
        ~?= "<:s> <:p> <:o> .\n"

    , "serializeGraph: empty graph gives empty string" ~:
        serializeGraph []
        ~?= ""

    , "evalSelect SQAll: returns all triples" ~:
        evalSelect (SQAll "people.ttl") sampleState
        ~?= [ ("<:Alice>",   "<:age>",      "30")
            , ("<:Bob>",     "<:age>",      "19")
            , ("<:Charlie>", "<:age>",      "21")
            , ("<:Alice>",   "<:worksFor>", "<:CompanyA>")
            ]

    , "evalSelect SQAll: missing graph returns empty list" ~:
        evalSelect (SQAll "missing.ttl") sampleState
        ~?= []

    , "evalSelect SQWhere: filters by >= on numeric object" ~:
        evalSelect
          (SQWhere "people.ttl"
            (FBase (FComp (Comp ">=" (SElemSimple EObject) (SELit "21")))))
          sampleState
        ~?= [ ("<:Alice>", "<:age>", "30")
            , ("<:Charlie>", "<:age>", "21")
            ]

    , "evalSelect SQWhere: filters by = on string object" ~:
        evalSelect
          (SQWhere "orgs.ttl"
            (FBase (FComp (Comp "=" (SElemSimple EObject) (SELit "London")))))
          sampleState
        ~?= [("<:CompanyA>", "<:locatedIn>", "London")]

    , "evalCombine CNested: same as evalSelect SQAll" ~:
        evalCombine (CNested (SQAll "people.ttl")) sampleState
        ~?= evalSelect (SQAll "people.ttl") sampleState

    , "evalCombine CCombine: merges two graphs, no duplicates" ~:
        length
          (evalCombine
            (CCombine (CNested (SQAll "people.ttl")) (SQAll "orgs.ttl"))
            sampleState)
        ~?= 5

    , "evalFilterStart FNot: inverts a passing filter" ~:
        let trueFilter  = FBase (FComp (Comp ">=" (SElemSimple EObject) (SELit "0")))
            notFilter   = FNot (FComp (Comp ">=" (SElemSimple EObject) (SELit "0")))
        in ( evalFilterStart trueFilter ("<:x>", "<:p>", "5") sampleState
           , evalFilterStart notFilter  ("<:x>", "<:p>", "5") sampleState
           )
        ~?= (True, False)

    , "evalFilterStart FAnd: both sides must be true" ~:
        let f = FAnd
                  (FComp (Comp ">=" (SElemSimple EObject) (SELit "20")))
                  (FFinal (Comp "<=" (SElemSimple EObject) (SELit "25")))
        in ( evalFilterStart f ("<:x>", "<:p>", "21") sampleState
           , evalFilterStart f ("<:x>", "<:p>", "30") sampleState
           )
        ~?= (True, False)
    ]

-- Entry point

main :: IO ()
main = do
    counts <- runTestTT $ TestList
        [ lexerTests
        , parserTests
        , normaliseTests
        , interpreterTests
        ]
    let total    = tried counts
        failures_ = failures counts
        errors_   = errors counts
    putStrLn $ show total ++ " tests run, "
            ++ show failures_ ++ " failures, "
            ++ show errors_   ++ " errors."
