{
module Grammar where
import Tokens
}


%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    nextLine { TokenNextLine _ }
    comment { TokenComment _ }
    Matrix { TokenMatrix _ }
    list   { TokenList _ $$ }
    int    { TokenInt _ $$ }
    Int    { TokenTInt _ }
    List   { TokenTList _ }
    or     {TokenOr _ }
    '+'    { TokenPlus _ }
    '-'    { TokenSubtract _ }
    '*'    { TokenMulti _ }
    '?'    { TokenQuest _ }
    ':'    { TokenCol _ }
    if     { TokenIf _ }
    else   { TokenElse _ }
    push   { TokenPush _ }
    pop    { TokenPop _ }
    getLists { TokenGetLists _ }
    '='    { TokenEq _ }
    '('    { TokenLParen _ }
    ')'    { TokenRParen _ }
    '{'    { TokenLBracket _ }
    '}'    { TokenRBracket _ }
    ';'    { TokenSeq _ }
    while  { TokenWhile _ }
    get    { TokenGet _ }
    empty { TokenEmpty _ }
    True   { TokenTrue _ }
    False { TokenFalse _ }
    return { TokenReturn _ }
    var    { TokenVar _ $$ }
    Bool   { TokenBool _ }


%left nextLine
%nonassoc '(' ')'
%nonassoc '{' '}'
%nonassoc if
%nonassoc then
%nonassoc else
%nonassoc '='

%left '+' '-' get empty or
%left '*' ';' '?' ':'
%nonassoc getLists
%nonassoc int var list matrix
%nonassoc while notAtEnd
%nonassoc True False
%nonassoc push pop
%nonassoc notAtEnd

%%
Statement :: { Statement }
Statement : Int var '=' Exp                                  { IntAssignment $2 $4 }
     | Bool var '=' Exp                                      { BoolAssignment $2 $4 }
     | List var '=' Exp                                      { ListAssignment $2 $4 }
     | Matrix var '=' getLists                               { MatrixAssignment $2 }
     | Exp or Exp                                            { OrStatement $1 $3 }
     | if '(' Exp ')' '{' nextLine Statement nextLine '}' else '{' nextLine Statement nextLine '}'      { IfThenElse $3 $7 $13 }
     | '(' Exp ')' '?' Statement ':' Statement               { IfThenElse $2 $5 $7 }
     | if '(' Exp ')' Statement                              { IfThen $3 $5 }
     | push var var                                          { Push $2  $3}
     | while '(' Exp ')' '{' nextLine Statement nextLine '}' { While $3 $7 }
     | Statement ';' Statement                               { Sequence $1 $3 }
     | return '(' var ')'                                    { Return $3 }
     | Statement nextLine Statement                          { Sequence $1 $3 }
     | Statement nextLine                                    { StatementLine $1 }
 

Exp :: {Expr}
Exp :     int                                   { CInt $1 }
        | var                                   { Var $1 }
        | getLists                              { GetLists }
        | empty var                             { Empty $2 }
        | Exp '+' Exp                           { Add $1 $3 }
        | Exp '-' Exp                           { Sub $1 $3 }
        | Exp '*' Exp                           { Mult $1 $3 }
        | '(' Exp ')'                           { $2 }
        | pop '(' var ')'                       { Pop $3 }
        | list                                  { CList $1 }
        | True                                  { CTrue }
        | False                                 { CFalse }
        | get '(' Exp ')' var                   { Get $3 $5 }

Type : Bool                     { TBool } 
     | Int                      { TInt } 
     | List                     { TList }
     | Matrix                   { TMatrix }

{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Statement = MatrixAssignment String
    | IntAssignment String Expr | OrStatement Expr Expr | BoolAssignment String Expr | ListAssignment String Expr
    | IfThenElse Expr Statement Statement
    | IfThen Expr Statement | While Expr Statement
    | Push String String | Sequence Statement Statement
    | Return String | StatementLine Statement
    deriving (Show,Eq)

data Expr = CInt Int | Var String
    | Add Expr Expr | Sub Expr Expr | Mult Expr Expr
    | GetLists | Pop String | Empty String
    | CList String | Get Expr String
    | CTrue | CFalse
    deriving (Show,Eq)

data LangType = TInt | TBool | TList | TMatrix
   deriving (Show,Eq)

}
