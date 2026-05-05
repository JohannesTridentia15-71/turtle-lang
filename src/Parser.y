{
module Parser where
import Lexer
}

%name parseTTL
%tokentype { TtlToken }
%error { parseError }

%token
    select_from         { TokenSelectFrom _ }
    select              { TokenSelect _ }
    where               { TokenWhere _ }
    select_object       { TokenSelectObject _ }
    from                { TokenFrom _ }
    combine             { TokenCombine _ }
    add                 { TokenAdd _ }
    to                  { TokenTo _ }
    in                  { TokenIn _ }
    and                 { TokenAnd _ }
    or                  { TokenOr _ }
    not                 { TokenNot _ }
    replace             { TokenReplace _ }
    replace_subject     { TokenReplaceSub _ }
    replace_predicate   { TokenReplacePred _ }
    replace_object      { TokenReplaceOb _ }
    with                { TokenWith _ }
    construct           { TokenConstruct _ }
    delete              { TokenDelete _ }
    start               { TokenStart _ }
    evaluate            { TokenEvaluate _ }
    union               { TokenUnion _ }
    intersection        { TokenIntersection _ }
    difference          { TokenDifference _ }
    min                 { TokenMin _ }
    max                 { TokenMax _ }
    count               { TokenCount _ }
    transitive_join     { TokenJoin _ }
    subject             { TokenSubjectElement _ }
    predicate           { TokenPredicateElement _ }
    object              { TokenObjectElement _ }
    branch              { TokenBranch _ $$ }
    graph_name          { TokenGraphName _ $$ }
    file_name           { TokenFileName _ $$ }
    element             { TokenElement _ $$ }
    arith_operand       { TokenArithOperator _ $$ }
    comp_operand        { TokenCompOperator _ $$ }
    pipe                { TokenPipe _ }
    save_to             { TokenSaveTo _ $$ }
    literal             { TokenLiteral _ $$ }
    uri_ref             { TokenURIRef _ $$ }

%left not
%nonassoc and or


%%

Top : Line1 { $1 }

Line1 
    : Line1 pipe Line   { LPipedQuery $1 $3 }
    | Line             { LBase $1 }

Line
    : Query save_to Identifier                 { LSaveQuery $1 $3 }
    | Query                                    { LNoSaveQuery $1  } 
    | evaluate Operation save_to file_name     { LEval $2 $4 }
    | evaluate Operation                       { LNoSaveEval $2} 

Query
    : CombineQuery                             { QCombine $1 }
    | AddQuery                                 { QAdd $1 }
    | ReplaceQuery                             { QReplace $1 }
    | construct                           { QConstruct }
    | DeleteQuery                              { QDelete $1 }

SelectQuery
    : select_from Identifier                   { SQAll $2 }
    | select_from Identifier where FilterStart  { SQWhere $2 $4 }
    | SelectElementQuery                       { SQElement $1 }

SelectEmptyQuery
    : select_from Identifier                   { SENoElemAll $2 }
    | select_from Identifier where FilterStart  { SENoElemWhere $2 $4 }

SelectElementQuery
    : select Element from Identifier where FilterStart  { SElem $2 $4 $6 }
    | Element                                           { SElemSimple $1 } 
    | literal                                           { SELit $1 }  
    | uri_ref                                           { SELit $1 }
    | Identifier                                        { SELit $1 } 

Element
    : subject                                  { ESubject }
    | predicate                                { EPredicate }
    | object                                   { EObject }

Identifier
    : graph_name                               { $1 }
    | file_name                                { $1 }
    | literal                                  { $1 }
    | uri_ref                                  { $1 }

SelectObjectQuery
    : select_object from Identifier where FilterStart { SObj $3 $5 }

CombineQuery
    : combine CombineQuery SelectQuery         { CCombine $2 $3 }
    | SelectQuery                              { CNested $1 }

AddQuery
    : add branch                             { AddQ $2 }
    | add uri_ref uri_ref uri_ref            { AddQ ($2 ++ " " ++ $3 ++ " " ++ $4) }
    | add uri_ref uri_ref literal            { AddQ ($2 ++ " " ++ $3 ++ " " ++ $4) }


ReplaceQuery
    : in SelectQuery replace_object with uri_ref
                                               { RqObject $2 $5 }
    | in SelectQuery replace_object with literal
                                               { RqObject $2 $5 }
    | in SelectQuery replace_predicate with uri_ref { RqPredicate $2 $5 }
    | in SelectQuery replace_subject with uri_ref   { RqSubject $2 $5 }


DeleteQuery
    : delete Identifier                        { Dq $2 }
    | delete Identifier where FilterStart      { DqWhere $2 $4 }

Operation
    : ArithmeticOperation                      { OpArith $1 }
    | ComparisonOperation                      { OpComp $1 }
    | GraphOperation                           { OpGraph $1 }
    | transitive_join Identifier Identifier where JoinFilter { OpJoin $2 $3 $5 }

JoinFilter
    : Element comp_operand Element { ($1, $2, $3) }

ArithmeticOperation
    : arith_operand SelectElementQuery SelectElementQuery
                                               { Arith $1 $2 $3 }

ComparisonOperation
    : comp_operand SelectElementQuery SelectElementQuery
                                               { Comp $1 $2 $3 }

GraphOperation
    : union SelectQuery SelectQuery
                                               { GUnion $2 $3 }
    | intersection SelectQuery SelectQuery
                                               { GIntersection $2 $3 }
    | difference SelectQuery SelectQuery
                                               { GDifference $2 $3 }
    | GraphOperationSingle                     { GSingle $1 }

GraphOperationSingle
    : min SelectQuery                   { GMin $2 }
    | max SelectQuery                   { GMax $2 }
    | count SelectQuery                 { GCount $2 }

FilterStart
    : start Filter and start FilterFinal                   { FAnd $2 $5 }
    | start Filter or start FilterFinal                    { FOr $2 $5 }
    | not Filter                               { FNot $2 }
    | Filter                                   { FBase $1 }

Filter
    : Filter and FilterFinal                   { FAnd' $1 $3 }
    | Filter or FilterFinal                    { FOr' $1 $3 }
    | not Filter                               { FNot' $2 }
    | ComparisonOperation                      { FComp $1 }

FilterFinal
    : ComparisonOperation                      { FFinal $1 }

{
-- Improved Error Handling: shows remaining tokens to help debugging
parseError :: [TtlToken] -> a
parseError [] = 
    error "Parse error: Unexpected end of input (EOF)."
parseError (t:ts) = 
    error ("Parse error at " ++ Lexer.tokenPosn t ++ " on token: " ++ show t)

data Line1
    = LPipedQuery Line1 Line 
    | LBase Line             
    deriving (Show, Eq)

data Line
    = LSaveQuery Query String
    | LNoSaveQuery Query
    | LEval Operation String
    | LNoSaveEval Operation
    deriving (Show, Eq)

data Query
    = QCombine CombineQuery
    | QAdd AddQuery
    | QReplace ReplaceQuery
    | QConstruct
    | QDelete DeleteQuery
    deriving (Show, Eq)

data SelectQuery
    = SQAll String
    | SQWhere String FilterStart
    | SQElement SelectElementQuery
    deriving (Show, Eq)

data SelectEmptyQuery
    = SENoElemAll String
    | SENoElemWhere String FilterStart
    deriving (Show, Eq)

data SelectElementQuery
    = SElem Element String FilterStart
    | SElemSimple Element
    | SELit String
    deriving (Show, Eq)

data Element
    = ESubject 
    | EPredicate 
    | EObject 
    deriving (Show, Eq)

data SelectObjectQuery
    = SObj String FilterStart
    deriving (Show, Eq)

data CombineQuery
    = CCombine CombineQuery SelectQuery
    | CNested SelectQuery
    deriving (Show, Eq)

data AddQuery
    = AddQ String
    deriving (Show, Eq)

data ReplaceQuery
    = RqObject SelectQuery String
    | RqPredicate SelectQuery String
    | RqSubject SelectQuery String
    deriving (Show, Eq)

data DeleteQuery
    = Dq String
    | DqWhere String FilterStart
    deriving (Show, Eq)

data Operation
    = OpArith ArithmeticOperation
    | OpComp ComparisonOperation
    | OpGraph GraphOperation
    | OpJoin String String (Element, String, Element)
    deriving (Show, Eq)

data ArithmeticOperation
    = Arith String SelectElementQuery SelectElementQuery
    deriving (Show, Eq)

data ComparisonOperation
    = Comp String SelectElementQuery SelectElementQuery
    deriving (Show, Eq)

data GraphOperation
    = GUnion SelectQuery SelectQuery
    | GIntersection SelectQuery SelectQuery
    | GDifference SelectQuery SelectQuery
    | GSingle GraphOperationSingle
    deriving (Show, Eq)

data GraphOperationSingle
    = GMin SelectQuery
    | GMax SelectQuery
    | GCount SelectQuery
    deriving (Show, Eq)

data FilterStart
    = FAnd Filter FilterFinal
    | FOr  Filter FilterFinal
    | FNot Filter
    | FBase Filter
    deriving (Show, Eq)

data Filter
    = FAnd' Filter FilterFinal
    | FOr'  Filter FilterFinal
    | FNot' Filter
    | FComp ComparisonOperation
    deriving (Show, Eq)

data FilterFinal
    = FFinal ComparisonOperation
    deriving (Show, Eq)
}