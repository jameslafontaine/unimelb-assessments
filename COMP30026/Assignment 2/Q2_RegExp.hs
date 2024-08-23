module RegExp (parseRE, numberOfSymb, RegExp(..))
where

-- --------------------------------------------
-- Regular expression type and top-level parser
-- --------------------------------------------

data RegExp
  = Symbol Char           -- Basic symbol (for simplicity, assume hex)
  | EmptyStr              -- Epsilon, the regexp. with language {ε}
  | EmptySet              -- Null, the regexp. with language {}
  | Concat RegExp RegExp  -- Concatenation
  | Union  RegExp RegExp  -- Union
  | Star   RegExp         -- Kleene star
  deriving (Eq, Show)

-- --------------------------------------------
-- Number of symbols in the regular expression
-- --------------------------------------------
numberOfSymb :: RegExp -> Int
numberOfSymb (EmptySet) = 1
numberOfSymb (EmptyStr) = 1
numberOfSymb (Symbol char) = 1
numberOfSymb (Union r s) = 1 + numberOfSymb r +  numberOfSymb s
numberOfSymb (Concat r s) = 1 + numberOfSymb r +  numberOfSymb s
numberOfSymb (Star r) = 1 + numberOfSymb r

parseRE :: String -> RegExp 
parseRE input
  = parse (scan input)

alphabet = "0123456789abcd"

-- ------
-- Lexing
-- ------

data Token
  = VBAR      -- vertical bar    |
  | STAR      -- Kleene star     *
  | OPEN      -- open paren      (
  | SHUT      -- close paren     )
  | ESTR      -- empty string    ε
  | ESET      -- empty set       ∅
  | CHAR Char -- literal char
  deriving Eq

instance Show Token where
  show VBAR = "'|'"
  show STAR = "'*'"
  show OPEN = "'('"
  show SHUT = "')'"
  show ESTR = "'ε'"
  show ESET = "'∅'"
  show (CHAR c) = show c

scan :: String -> [Token]
-- base case:
scan [] = []
-- skip whitespace:
scan (' ':cs)  = scan cs
scan ('\t':cs) = scan cs
scan ('\n':cs) = scan cs
-- handle character groups:
scan ('e':'p':'s':cs)
  = ESTR : scan cs
scan ('n':'u':'l':'l':cs)
  = ESET : scan cs
-- everything else as normal:
scan (c:cs)
  = token c : scan cs
    where
      token '|' = VBAR
      token '*' = STAR
      token '(' = OPEN
      token ')' = SHUT
      token 'ε' = ESTR
      token '∅' = ESET
      token  x
        | elem x alphabet = CHAR x
        | otherwise       = error $ "invalid character: " ++ show x

-- -------
-- Parsing
-- -------

--  Here is a context-free grammar for the input language:
--
--    S  ->  P S'                             (unions)
--    S' ->  ε | "|" P S'
--    P  ->  F P'                             (catenations)
--    P' ->  ε | F P' 
--    F  ->  B F'                             (factors, possibly starred)
--    F' ->  ε | "*" F'
--    B  ->  char | eps | null | "(" S ")"    (basic factors)
--
--  A parser takes a list of characters. The parsers for S, P, F and B 
--  consume characters greedily in an attempt to recognise well-formed 
--  regexps of syntactic category S, P, F and B, respectively. When one of 
--  these parsers finds such a (sub-)regexp r, it returns r, together with 
--  the remaining (un-consumed) characters.
--  The parsers for S', P' and F' find snippets such as "| ..." and "*",
--  that is, operations that have to be applied to an already parsed regexp.
--  These parsers should therefore return functions, or regexp transformers.
--  For example, the parser for F' checks whether an expression found to be 
--  a valid B (a "basic factor") has a Kleene star attached to it. The parser
--  produces the wrapper 'Star' if a star is attached, otherwise no wrapper 
--  (that is, it uses the identity function id as a wrapper).
--  These differences explain the use of a type variable in the Parser type
--  below. A parser of type `Parser a' is a parser returning objects of
--  type `a' (in addition to the remaining un-consumed characters).

type Parser a = [Token] -> (a, [Token])

parse :: [Token] -> RegExp
parse input
  | null leftovers = r
  | otherwise      = error $ "trailing characters: " ++ show leftovers
  where
    (r, leftovers) = parseS input

--  We name the individual parsers after their syntactic categories.
--  The parser parseS recognises an arbitrary regular expression.

parseS, parseP, parseF :: Parser RegExp

parseS input
  = (wrapper r, input2)
    where
      (r, input1)       = parseP input
      (wrapper, input2) = parseS' input1

parseP input
  = (wrapper r, input2)
    where
      (r, input1)       = parseF input
      (wrapper, input2) = parseP' input1

parseF input
  = (wrapper r, input2)
    where
      (r, input1)       = parseB input
      (wrapper, input2) = parseF' input1

parseB (ESET : input)
  = (EmptySet, input)
parseB (ESTR : input)
  = (EmptyStr, input)
parseB (OPEN : input)
  = (r, shut input1)
    where
      (r, input1) = parseS input
      shut s
        | null s         = error $ "expected ')', found end of input"
        | head s /= SHUT = error $ "expected ')', found " ++ show s
        | otherwise      = tail s
parseB ((CHAR c):toks)
  = (Symbol c, toks)
parseB []
  = error $ "expected '(', symbol, 'eps', or 'null', found end of input"
parseB (tok:input)
  = error $ "expected '(', symbol, 'eps', or 'null', found " ++ show tok

parseS', parseP', parseF' :: Parser (RegExp -> RegExp)

parseS' (VBAR : input)
  = (\r -> Union r (wrapper r1), input2)
    where
      (r1, input1)      = parseP  input
      (wrapper, input2) = parseS' input1
parseS' input
  = (id, input)

parseP' input
  | null input                     = (id, [])
  | head input `elem` [VBAR, SHUT] = (id, input)
  | otherwise                      = (\r -> Concat r (wrapper r1), input2)
    where
      (r1, input1)      = parseF  input
      (wrapper, input2) = parseP' input1

parseF' (STAR : input)
  = (Star . wrapper, input1)
    where
      (wrapper, input1) = parseF' input
parseF' input
  = (id, input)


