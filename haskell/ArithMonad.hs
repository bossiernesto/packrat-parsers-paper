-- Packrat parser for trivial arithmetic language.
module ArithPackrat where

import Pos
import Parse


data ArithDerivs = ArithDerivs {
		dvAdditive	:: Result ArithDerivs Int,
		dvMultitive	:: Result ArithDerivs Int,
		dvPrimary	:: Result ArithDerivs Int,
		dvDecimal	:: Result ArithDerivs Int,
		advChar		:: Result ArithDerivs Char,
		advPos		:: Pos
	}

instance Derivs ArithDerivs where
	dvChar d = advChar d
	dvPos d = advPos d


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.
eval s = case dvAdditive (parse (Pos "<input>" 1 1) s) of
		Parsed v d' e' -> v
		_ -> error "Parse error"


-- Construct a (lazy) parse result structure for an input string,
-- in which any result can be computed in linear time
-- with respect to the length of the input.
parse :: Pos -> String -> ArithDerivs
parse pos s = d where
    d    = ArithDerivs add mult prim dec chr pos
    add  = pAdditive d
    mult = pMultitive d
    prim = pPrimary d
    dec  = pDecimal d
    chr  = case s of
             (c:s') -> Parsed c (parse (nextPos pos c) s') (nullError d)
             [] -> NoParse (eofError d)


-- Parse an additive-precedence expression
pAdditive :: ArithDerivs -> Result ArithDerivs Int
Parser pAdditive =
	    (do vleft <- Parser dvMultitive
		char '+'
		vright <- Parser dvAdditive
		return (vleft + vright))
	<|> (do Parser dvMultitive)

-- Parse a multiplicative-precedence expression
pMultitive :: ArithDerivs -> Result ArithDerivs Int
Parser pMultitive =
	    (do vleft <- Parser dvPrimary
		char '*'
		vright <- Parser dvMultitive
		return (vleft * vright))
	<|> (do Parser dvPrimary)

-- Parse a primary expression
pPrimary :: ArithDerivs -> Result ArithDerivs Int
Parser pPrimary =
	    (do char '('
		vexp <- Parser dvAdditive
		char ')'
		return vexp)
	<|> (do Parser dvDecimal)

-- Parse a decimal digit
pDecimal :: ArithDerivs -> Result ArithDerivs Int
Parser pDecimal =
	    (do c <- digit
		return (digitToInt c))


