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
    subject             { TokenSubjectElement _ }
    predicate           { TokenPredicateElement _ }
    object              { TokenObjectElement _ }
    branch              { TokenBranch _ $$ }
    graph_name          { TokenGraphName _ $$ }
    file_name           { TokenFileName _ $$ }
    element             { TokenElement _ $$ }
    arith_operand       { TokenArithOperator _ $$ }
    comp_operand        { TokenCompOperator _ $$ }
    save_to             { TokenSaveTo _ $$ }
    literal             { TokenLiteral _ $$ }

%left not
%nonassoc and or


%%

Line
    : Query save_to graph_name                 { LSave $1 $3 }
    | evaluate Operation save_to file_name     { LEval $2 $4 }

Query
    : CombineQuery                             { QCombine $1 }
    | AddQuery                                 { QAdd $1 }
    | ReplaceQuery                             { QReplace $1 }
    | ConstructQuery                           { QConstruct $1 }
    | DeleteQuery                              { QDelete $1 }

SelectQuery
    : select_from graph_name                   { SQAll $2 }
    | select_from graph_name where FilterStart { SQWhere $2 $4 }
    | SelectElementQuery                       { SQElement $1 }

SelectEmptyQuery
    : select_from graph_name                   { SENoElemAll $2 }
    | select_from graph_name where FilterStart { SENoElemWhere $2 $4 }

SelectElementQuery
    : select Element from graph_name where FilterStart  { SElem $2 $4 $6 }
    | literal                                  { SELit $1 }

Element
    : subject                                  { ESubject }
    | predicate                                { EPredicate  }
    | object                                   { EObject  }

SelectObjectQuery
    : select_object from graph_name where FilterStart { SObj $3 $5 }

CombineQuery
    : combine CombineQuery SelectQuery         { CCombine $2 $3 }
    | SelectQuery                              { CNested $1 }

AddQuery
    : add branch to graph_name                 { AddQ $2 $4 }

ReplaceQuery
    : in graph_name replace SelectEmptyQuery with element
                                              { RqObject $2 $4 $6 }
    | in graph_name replace SelectEmptyQuery with SelectObjectQuery
                                              { RqSelectObject $2 $4 $6 }

ConstructQuery
    : construct graph_name                     { Cq $2 }

DeleteQuery
    : delete graph_name                        { Dq $2 }
    | delete graph_name where FilterStart      { DqWhere $2 $4 }

Operation
    : ArithmeticOperation                      { OpArith $1 }
    | ComparisonOperation                      { OpComp $1 }
    | GraphOperation                           { OpGraph $1 }

ArithmeticOperation
    : arith_operand SelectElementQuery SelectElementQuery
                                              { Arith $1 $2 $3 }

ComparisonOperation
    : comp_operand SelectElementQuery SelectElementQuery
                                              { Comp $1 $2 $3 }

GraphOperation
    : union SelectElementQuery SelectElementQuery
                                              { GUnion $2 $3 }
    | intersection SelectElementQuery SelectElementQuery
                                              { GIntersection $2 $3 }
    | difference SelectElementQuery SelectElementQuery
                                              { GDifference $2 $3 }
    | GraphOperationSingle                     { GSingle $1 }

GraphOperationSingle
    : min SelectElementQuery                   { GMin $2 }
    | max SelectElementQuery                   { GMax $2 }

FilterStart
    : start Filter and start FilterFinal                   { FAnd $2 $5 }
    | start Filter or start FilterFinal                    { FOr $2 $5 }
    | not Filter                               { FNot $2 }

Filter
    : Filter and FilterFinal                   { FAnd' $1 $3 }
    | Filter or FilterFinal                    { FOr' $1 $3 }
    | not Filter                               { FNot' $2 }
    | ComparisonOperation                      { FComp $1 }

FilterFinal
    : ComparisonOperation                      { FFinal $1 }

{
parseError :: [TtlToken] -> a
parseError []     = error "Unknown Parse Error"
parseError (t:ts) =
  error ("Parse error at line:column " ++ (tokenPosn t) ++ " : " ++ show ts)

data Line
    = LSave Query String
    | LEval Operation String
    deriving (Show, Eq)

data Query
    = QCombine CombineQuery
    | QAdd AddQuery
    | QReplace ReplaceQuery
    | QConstruct ConstructQuery
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
    = AddQ String String
    deriving (Show, Eq)

data ReplaceQuery
    = RqObject String SelectEmptyQuery String
    | RqSelectObject String SelectEmptyQuery SelectObjectQuery
    deriving (Show, Eq)

data ConstructQuery
    = Cq String
    deriving (Show, Eq)

data DeleteQuery
    = Dq String
    | DqWhere String FilterStart
    deriving (Show, Eq)

data Operation
    = OpArith ArithmeticOperation
    | OpComp ComparisonOperation
    | OpGraph GraphOperation
    deriving (Show, Eq)

data ArithmeticOperation
    = Arith String SelectElementQuery SelectElementQuery
    deriving (Show, Eq)

data ComparisonOperation
    = Comp String SelectElementQuery SelectElementQuery
    deriving (Show, Eq)

data GraphOperation
    = GUnion SelectElementQuery SelectElementQuery
    | GIntersection SelectElementQuery SelectElementQuery
    | GDifference SelectElementQuery SelectElementQuery
    | GSingle GraphOperationSingle
    deriving (Show, Eq)

data GraphOperationSingle
    = GMin SelectElementQuery
    | GMax SelectElementQuery
    deriving (Show, Eq)

data FilterStart
    = FAnd Filter FilterFinal
    | FOr  Filter FilterFinal
    | FNot Filter
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