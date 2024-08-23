module Exp (parse, Exp(..))
where

import Data.Char

-- ----------------------------------------------------------------------
--
-- Structure Trees
--
-- ----------------------------------------------------------------------

-- The type of structure trees:

data Exp
  = VAR Char           -- Variable
  | NOT Exp            -- Negation
  | AND Exp Exp        -- Conjunction
  | OR Exp Exp         -- Disjunction
  | XOR Exp Exp        -- Exclusive or
  | BIIM Exp Exp       -- Biimplication
  | IMPL Exp Exp       -- Only if (=>)
  deriving (Eq, Show)

-- ----------------------------------------------------------------------
--
-- Lexical analysis
--
-- ----------------------------------------------------------------------

data Token
  = Ident Char         -- Propositional letter
  | NegToken           -- Negation (~)
  | AndToken           -- Conjunction (&)
  | OrToken            -- Disjunction (|)
  | XorToken           -- Exclusive or (<+>)
  | BiimToken          -- Biimplication (<=>)
  | ImplToken          -- Only if (=>)
  | LeftParen          -- Left parenthesis
  | RightParen         -- Right parenthesis
  | Bad String         -- Ill-formed text
  deriving (Eq, Show)

-- Tokenize the input. Lexical errors end up as Bad tokens.

scan :: String -> [Token]

scan (c:s) | isSpace c = scan s
scan (c:s) | isUpper c = Ident c : scan s
scan ('~':s)           = NegToken : scan s
scan ('&':s)           = AndToken : scan s
scan ('|':s)           = OrToken : scan s
scan ('<':'+':'>':s)   = XorToken : scan s
scan ('<':'=':'>':s)   = BiimToken : scan s
scan ('=':'>':s)       = ImplToken : scan s
scan ('(':s)           = LeftParen : scan s
scan (')':s)           = RightParen : scan s
scan []                = []
scan s                 = Bad stuff : scan rest
                         where
                           (stuff,rest) = break isSpace s

-- ----------------------------------------------------------------------
--
-- Parsing
--
-- ----------------------------------------------------------------------

-- Here is a context-free grammar for the input language:
--
--   exp -> var
--       |  ( ~ exp )
--       |  ( exp & exp )
--       |  ( exp | exp )
--       |  ( exp => exp )
--       |  ( exp <=> exp )
--       |  ( exp <+> exp )
--       |  ( exp )

-- A parser takes a list of tokens.  It takes as many of these as
-- needed to produce a well-formed Exp.  This expression is then
-- returned, together with the remaining (unconsumed) tokens.
-- But parsers may also return objects more complex than elements
-- of Exp (see pRem below).  This explains the use of a type
-- variable - a parser of type `Parser a' is a parser returning
-- objects of type `a' (in addition to the remaining tokens).

type Parser a = [Token] -> (a,[Token])

parse :: String -> Exp
parse input
  = if null remaining_tokens then exp
      else error ("Trailing garbage: " ++ show remaining_tokens)
    where
      tokens = scan input
      (exp, remaining_tokens) = pExp tokens

-- -------------------------------------------------
pExp :: Parser Exp

-- Parse an expression, including possible parentheses.
-- -------------------------------------------------

pExp (Ident c : tokens)
  = (VAR c, tokens)

pExp (LeftParen : tokens)
  = (exp, consume_RightParen remaining_tokens)
    where
      (exp, remaining_tokens) = pPar tokens
      consume_RightParen ts
        | null ts             = error "Unexpected end of input"
        | next /= RightParen  = error ("RightParen expected, found " ++ show next)
        | otherwise           = tail ts
          where
            next = head ts

pExp tokens
  = error ("Identifier or left parenthesis expected, found " ++ show next)
    where
      next = head tokens

-- -------------------------------------------------
pPar :: Parser Exp

-- Parse an expression without its enclosing parentheses.
-- -------------------------------------------------

pPar (NegToken : tokens)
  = (NOT exp, remaining_tokens)
    where
      (exp, remaining_tokens) = pExp tokens

pPar tokens
  = (complete exp, remaining_tokens)
    where
      (exp, intermediate_tokens) = pExp tokens
      (complete, remaining_tokens) = pRem intermediate_tokens

-- -------------------------------------------------
pRem :: Parser (Exp -> Exp)

-- Parse a connective C, followed by its second argument.
-- Return a function which, when given the first argument,
-- will produce the expression for C.
-- -------------------------------------------------

pRem (AndToken : tokens)
  = (\e1 -> AND e1 e2, remaining_tokens)
    where
      (e2, remaining_tokens) = pExp tokens

pRem (OrToken : tokens)
  = (\e1 -> OR e1 e2, remaining_tokens)
    where
      (e2, remaining_tokens) = pExp tokens

pRem (XorToken : tokens)
  = (\e1 -> XOR e1 e2, remaining_tokens)
    where
      (e2, remaining_tokens) = pExp tokens

pRem (BiimToken : tokens)
  = (\e1 -> BIIM e1 e2, remaining_tokens)
    where
      (e2, remaining_tokens) = pExp tokens

pRem (ImplToken : tokens)
  = (\e1 -> IMPL e1 e2, remaining_tokens)
    where
      (e2, remaining_tokens) = pExp tokens

pRem (RightParen : tokens)
  = (id, tokens)

pRem (t : _)
  = error ("Unexpected token: " ++ show t)

pRem []
  = error "Unexpected end of input, expected connective or RightParen"
