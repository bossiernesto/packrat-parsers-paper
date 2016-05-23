-- Recursive descent parser for trivial arithmetic language
-- supporting only addition and multiplication operators.
module ArithRecurse where


data Result v =
	  Parsed v String
	| NoParse


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.
eval s = case pAdditive s of
		Parsed v rem -> v
		_ -> error "Parse error"


-- Parse an additive-precedence expression
pAdditive :: String -> Result Int
pAdditive s = alt1 where

    -- Additive <- Multitive '+' Additive
    alt1 = case pMultitive s of
	     Parsed vleft s' ->
	       case s' of
		 ('+':s'') ->
		   case pAdditive s'' of
		     Parsed vright s''' ->
		       Parsed (vleft + vright) s'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Additive <- Multitive
    alt2 = case pMultitive s of
	     Parsed v s' -> Parsed v s'
	     NoParse -> NoParse


-- Parse a multiplicative-precedence expression
pMultitive :: String -> Result Int
pMultitive s = alt1 where

    -- Multitive <- Primary '*' Multitive
    alt1 = case pPrimary s of
	     Parsed vleft s' ->
	       case s' of
		 ('*':s'') ->
		   case pMultitive s'' of
		     Parsed vright s''' ->
		       Parsed (vleft * vright) s'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Multitive <- Primary
    alt2 = case pPrimary s of
	     Parsed v s' -> Parsed v s'
	     NoParse -> NoParse

-- Parse a primary expression
pPrimary :: String -> Result Int
pPrimary s = alt1 where

    -- Primary <- '(' Additive ')'
    alt1 = case s of
	     ('(':s') ->
	       case pAdditive s' of
	         Parsed v s'' ->
	           case s'' of
	             (')':s''') -> Parsed v s'''
	             _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Primary <- Decimal
    alt2 = case pDecimal s of
	     Parsed v s' -> Parsed v s'
	     NoParse -> NoParse

-- Parse a decimal digit
pDecimal :: String -> Result Int
pDecimal ('0':cs) = Parsed 0 cs
pDecimal ('1':cs) = Parsed 1 cs
pDecimal ('2':cs) = Parsed 2 cs
pDecimal ('3':cs) = Parsed 3 cs
pDecimal ('4':cs) = Parsed 4 cs
pDecimal ('5':cs) = Parsed 5 cs
pDecimal ('6':cs) = Parsed 6 cs
pDecimal ('7':cs) = Parsed 7 cs
pDecimal ('8':cs) = Parsed 8 cs
pDecimal ('9':cs) = Parsed 9 cs
pDecimal _ = NoParse


