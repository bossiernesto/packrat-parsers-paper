-- Packrat parser for trivial arithmetic language.
module ArithPackrat where


data Result v =
	  Parsed v Derivs
	| NoParse

data Derivs = Derivs {
		dvAdditive	:: Result Int,
		dvMultitive	:: Result Int,
		dvPrimary	:: Result Int,
		dvDecimal	:: Result Int,
		dvChar		:: Result Char
	}


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.
eval s = case dvAdditive (parse s) of
		Parsed v rem -> v
		_ -> error "Parse error"


-- Construct a (lazy) parse result structure for an input string,
-- in which any result can be computed in linear time
-- with respect to the length of the input.
parse :: String -> Derivs
parse s = d where
    d    = Derivs add mult prim dec chr
    add  = pAdditive d
    mult = pMultitive d
    prim = pPrimary d
    dec  = pDecimal d
    chr  = case s of
             (c:s') -> Parsed c (parse s')
             [] -> NoParse


-- Parse an additive-precedence expression
pAdditive :: Derivs -> Result Int
pAdditive d = alt1 where

    -- Additive <- Multitive '+' Additive
    alt1 = case dvMultitive d of
	     Parsed vleft d' ->
	       case dvChar d' of
		 Parsed '+' d'' ->
		   case dvAdditive d'' of
		     Parsed vright d''' ->
		       Parsed (vleft + vright) d'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Additive <- Multitive
    alt2 = case dvMultitive d of
	     Parsed v d' -> Parsed v d'
	     NoParse -> NoParse


-- Parse a multiplicative-precedence expression
pMultitive :: Derivs -> Result Int
pMultitive d = alt1 where

    -- Multitive <- Primary '*' Multitive
    alt1 = case dvPrimary d of
	     Parsed vleft d' ->
	       case dvChar d' of
		 Parsed '*' d'' ->
		   case dvMultitive d'' of
		     Parsed vright d''' ->
		       Parsed (vleft * vright) d'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Multitive <- Primary
    alt2 = case dvPrimary d of
	     Parsed v d' -> Parsed v d'
	     NoParse -> NoParse

-- Parse a primary expression
pPrimary :: Derivs -> Result Int
pPrimary d = alt1 where

    -- Primary <- '(' Additive ')'
    alt1 = case dvChar d of
	     Parsed '(' d' ->
	       case dvAdditive d' of
	         Parsed v d'' ->
	           case dvChar d'' of
	             Parsed ')' d''' -> Parsed v d'''
	             _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Primary <- Decimal
    alt2 = case dvDecimal d of
	     Parsed v d' -> Parsed v d'
	     NoParse -> NoParse

-- Parse a decimal digit
pDecimal :: Derivs -> Result Int
pDecimal d = case dvChar d of
		Parsed '0' d' -> Parsed 0 d'
		Parsed '1' d' -> Parsed 1 d'
		Parsed '2' d' -> Parsed 2 d'
		Parsed '3' d' -> Parsed 3 d'
		Parsed '4' d' -> Parsed 4 d'
		Parsed '5' d' -> Parsed 5 d'
		Parsed '6' d' -> Parsed 6 d'
		Parsed '7' d' -> Parsed 7 d'
		Parsed '8' d' -> Parsed 8 d'
		Parsed '9' d' -> Parsed 9 d'
		_ -> NoParse


