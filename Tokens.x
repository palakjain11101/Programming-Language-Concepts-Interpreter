{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabetic characters
$white = [\ \t\f\v\r]

tokens :-
  "--".*\n        ; 
  \n            { tok (\p s -> TokenNextLine p) }
  or            { tok (\p s -> TokenOr p) }
  $white+       ;
  Matrix         { tok (\p s -> TokenMatrix p) }
  return         { tok (\p s -> TokenReturn p) }
  List           { tok (\p s -> TokenTList p) }
  \[ [$digit \,]* \]   { tok (\p s -> TokenList p s) }
  $digit+       { tok (\p s -> TokenInt p (read s)) }
  Int           { tok (\p s -> TokenTInt p ) }
  \+             { tok (\p s -> TokenPlus p) }
  \-             { tok (\p s -> TokenSubtract  p) }
  \*             { tok (\p s -> TokenMulti p) }
  if             { tok (\p s -> TokenIf p) }
  \e\l\s\e           { tok (\p s -> TokenElse p) }
  push           { tok (\p s -> TokenPush p) }
  \?              { tok (\p s -> TokenQuest p) }
  \:             { tok (\p s -> TokenCol p) }
  pop            { tok (\p s -> TokenPop p) }
  getLists       { tok (\p s -> TokenGetLists p) }
  =              { tok (\p s -> TokenEq p )}
  \(             { tok (\p s -> TokenLParen p) }
  \)             { tok (\p s -> TokenRParen p) }
  \{             { tok (\p s -> TokenLBracket p) }
  \}             { tok (\p s -> TokenRBracket p) }
  \;              { tok (\p s -> TokenSeq p) }
  while          { tok (\p s -> TokenWhile p) }
  get            { tok (\p s -> TokenGet p ) }
  empty         { tok (\p s -> TokenEmpty p ) }
  True          { tok (\p s -> TokenTrue p) }
  False         { tok (\p s -> TokenFalse p) }
  Bool           { tok (\p s -> TokenBool p) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) }

{

-- Helper function
tok f p s = f p s

-- The token type:
data Token =
  TokenNextLine AlexPosn         |
  TokenInt AlexPosn Int          |
  TokenComment AlexPosn          |
  TokenMatrix AlexPosn           |
  TokenList AlexPosn String      |
  TokenOr AlexPosn               |
  TokenQuest AlexPosn            |
  TokenCol AlexPosn              |
  TokenPlus AlexPosn             |
  TokenTList AlexPosn            |
  TokenSubtract AlexPosn         |
  TokenMulti AlexPosn            |
  TokenIf AlexPosn               |
  TokenElse AlexPosn             |
  TokenPush AlexPosn             |
  TokenPop  AlexPosn             |
  TokenGetLists AlexPosn         |
  TokenEq AlexPosn               |
  TokenLParen AlexPosn           |
  TokenRParen AlexPosn           |
  TokenLBracket AlexPosn         |
  TokenRBracket AlexPosn         |
  TokenRepeat AlexPosn           |
  TokenGet AlexPosn              |
  TokenSeq AlexPosn              |
  TokenWhile AlexPosn            |
  TokenTrue AlexPosn             |
  TokenFalse AlexPosn            |
  TokenEmpty AlexPosn            |
  TokenBool AlexPosn             |
  TokenTInt AlexPosn             |
  TokenReturn AlexPosn           |
  TokenVar AlexPosn String
  deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenNextLine  (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "new line error"
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c) ++ "Int Error"
tokenPosn (TokenList  (AlexPn a l c) _ ) = show(l) ++ ":" ++ show(c) ++ "List Error"
tokenPosn (TokenMatrix (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ "Matrix error"
tokenPosn (TokenOr  (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Or Error"
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Plus Error"
tokenPosn (TokenSubtract (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Substact Error"
tokenPosn (TokenMulti  (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Mult Error"
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)  ++ " If Error"
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Else Error"
tokenPosn (TokenPush (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Push Error"
tokenPosn (TokenPop (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Pop Error"
tokenPosn (TokenGetLists (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "getLists Error"
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Equals Error"
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Left Parenthesis  Error"
tokenPosn (TokenTList (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "List Type Error"
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Right Parenthesis Error"
tokenPosn (TokenLBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Left Bracket  Error"
tokenPosn (TokenRBracket (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Right Bracket Error"
tokenPosn (TokenRepeat (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Repeat Error"
tokenPosn (TokenQuest (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Question Mark Error"
tokenPosn (TokenCol (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Colon Error"
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c) ++ "Var Error"
tokenPosn (TokenGet (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ "Get Error"
tokenPosn (TokenSeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Seq Error"
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " While Error"
tokenPosn (TokenEmpty (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " Empty Error"
tokenPosn (TokenTInt (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " TInt Error"
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " True Error"
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ " False Error"
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Return Error"
tokenPosn (TokenComment (AlexPn a l c)) = show(l) ++ ":" ++ show(c) ++ "Comment Error"
tokenPosn (TokenBool (AlexPn a l c) ) = show(l) ++ ":" ++ show(c) ++ " Bool Error"
}
