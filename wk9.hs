module Submission where

data Tokens = AToken | LParen | RParen | Comma | Error String | EOF deriving (Eq, Show)

token :: Char -> Tokens
token 'a' = AToken
token '(' = LParen
token ')' = RParen
token ',' = Comma
token b = Error "Invalid Token"

getTokens :: [Char] -> [Tokens]
getTokens [] = []
getTokens [b] = token b : [EOF]
getTokens (b : xb) = token b : getTokens xb

data Exp = A | AST_Tuple [Exp] | AST_Error String deriving (Eq, Show)

parseExp :: [Tokens] -> (Exp, [Tokens])
parseExp (AToken : xt) = (A, xt)
parseExp (LParen : xt) =
  let (eTree, rest) = parseTuple xt
   in if head rest == RParen
        then (AST_Tuple eTree, tail rest)
        else (AST_Error "No Closing Paren ", [EOF])
parseExp _ = (AST_Error "Parse Exp Error ", [EOF])

parseTuple :: [Tokens] -> ([Exp], [Tokens])
parseTuple (AToken : xt) =
  let (eL, rest) = parseExp (AToken : xt)
   in parseExpTail ([eL], rest)
parseTuple (LParen : xt) =
  let (eL, rest) = parseExp (LParen : xt)
   in parseExpTail ([eL], rest)
parseTuple _ = ([AST_Error "Parse Tuple Error "], [])

parseExpTail :: ([Exp], [Tokens]) -> ([Exp], [Tokens])
parseExpTail (e, Comma : xt) =
  let (eL, rest) = parseExp xt
   in parseExpTail (e ++ [eL], rest)
parseExpTail (e, RParen : xt) = (e, RParen : xt)
parseExpTail (e, EOF : xt) = (e, EOF : xt)
parseExpTail (_, _) = ([AST_Error "Parse ExpTail Error "], [])

parseTokens :: [Tokens] -> Exp
parseTokens t =
  let (ast1, rest) = parseExp t
   in if rest == [EOF]
        then ast1
        else AST_Error "More Input than expected"

parse :: [Char] -> Exp
parse l = parseTokens (getTokens l)