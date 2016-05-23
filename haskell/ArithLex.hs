-- Packrat parser for arithmetic expression language
-- with integrated lexical analysis
module ArithLex where


data Result v =
	  Parsed v Derivs
	| NoParse

data Derivs = Derivs {
		dvAdditive		:: Result Int,
		dvAdditiveSuffix	:: Result (Int -> Int),
		dvMultitive		:: Result Int,
		dvMultitiveSuffix	:: Result (Int -> Int),
		dvPrimary		:: Result Int,
		dvDecimal		:: Result Int,
		dvDigits		:: Result (Int, Int),
		dvDigit			:: Result Int,
		dvSymbol		:: Result Char,
		dvWhitespace		:: Result (),
		dvChar			:: Result Char
	}


-- Evaluate an expression and return the unpackaged result,
-- ignoring any unparsed remainder.
eval s = case dvWhitespace (parse s) of
	   Parsed _ d' ->
	     case dvAdditive d' of
		Parsed v d'' ->
		  case dvChar d'' of
		    Parsed _ _ -> error "Incomplete parse"
		    _ -> v
		_ -> error "Parse error"


-- Construct a (lazy) parse result structure for an input string,
-- in which any result can be computed in linear time
-- with respect to the length of the input.
parse :: String -> Derivs
parse s = d where
    d        = Derivs add addsuff mult multsuff prim
		      dec digs dig sym white chr
    add      = pAdditive d
    addsuff  = pAdditiveSuffix d
    mult     = pMultitive d
    multsuff = pMultitiveSuffix d
    prim     = pPrimary d
    dec      = pDecimal d
    digs     = pDigits d
    dig      = pDigit d
    sym     = pSymbol d
    white    = pWhitespace d
    chr      = case s of
                 (c:s') -> Parsed c (parse s')
                 [] -> NoParse


-- Parse an additive-precedence expression
-- Additive <- Multitive AdditiveSuffix
pAdditive :: Derivs -> Result Int
pAdditive d = case dvMultitive d of
		Parsed vleft d' ->
		  case dvAdditiveSuffix d' of
		    Parsed vsuff d'' ->
		      Parsed (vsuff vleft) d''
		    _ -> NoParse
		_ -> NoParse

-- Parse an additive-precedence expression
pAdditiveSuffix :: Derivs -> Result (Int -> Int)
pAdditiveSuffix d = alt1 where

    -- AdditiveSuffix <- '+' Multitive AdditiveSuffix
    alt1 = case dvSymbol d of
	     Parsed '+' d' ->
	       case dvMultitive d' of
		 Parsed vright d'' ->
		   case dvAdditiveSuffix d'' of
		     Parsed vsuff d''' ->
		       Parsed (\vleft -> vsuff (vleft + vright)) d'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- AdditiveSuffix <- '-' Multitive AdditiveSuffix
    alt2 = case dvSymbol d of
	     Parsed '-' d' ->
	       case dvMultitive d' of
		 Parsed vright d'' ->
		   case dvAdditiveSuffix d'' of
		     Parsed vsuff d''' ->
		       Parsed (\vleft -> vsuff (vleft - vright)) d'''
		     _ -> alt3
		 _ -> alt3
	     _ -> alt3

    -- AdditiveSuffix <- <empty>
    alt3 = Parsed (\v -> v) d


-- Parse a multiplicative-precedence expression
-- Multitive <- Primary MultitiveSuffix
pMultitive :: Derivs -> Result Int
pMultitive d = case dvPrimary d of
		 Parsed vleft d' ->
		   case dvMultitiveSuffix d' of
		     Parsed vsuff d'' ->
		       Parsed (vsuff vleft) d''
		     _ -> NoParse
		 _ -> NoParse

-- Parse a multiplicative-precedence expression
pMultitiveSuffix :: Derivs -> Result (Int -> Int)
pMultitiveSuffix d = alt1 where

    -- MultitiveSuffix <- '*' Primary MultitiveSuffix
    alt1 = case dvSymbol d of
	     Parsed '*' d' ->
	       case dvPrimary d' of
		 Parsed vright d'' ->
		   case dvMultitiveSuffix d'' of
		     Parsed vsuff d''' ->
		       Parsed (\vleft -> vsuff (vleft * vright)) d'''
		     _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- MultitiveSuffix <- '/' Primary MultitiveSuffix
    alt2 = case dvSymbol d of
	     Parsed '/' d' ->
	       case dvPrimary d' of
		 Parsed vright d'' ->
		   case dvMultitiveSuffix d'' of
		     Parsed vsuff d''' ->
		       Parsed (\vleft -> vsuff (vleft `div` vright)) d'''
		     _ -> alt3
		 _ -> alt3
	     _ -> alt3

    -- MultitiveSuffix <- '%' Primary MultitiveSuffix
    alt3 = case dvSymbol d of
	     Parsed '%' d' ->
	       case dvPrimary d' of
		 Parsed vright d'' ->
		   case dvMultitiveSuffix d'' of
		     Parsed vsuff d''' ->
		       Parsed (\vleft -> vsuff (vleft `mod` vright)) d'''
		     _ -> alt4
		 _ -> alt4
	     _ -> alt4

    -- MultitiveSuffix <- <empty>
    alt4 = Parsed (\v -> v) d

-- Parse a primary expression
pPrimary :: Derivs -> Result Int
pPrimary d = alt1 where

    -- Primary <- '(' Additive ')'
    alt1 = case dvSymbol d of
	     Parsed '(' d' ->
	       case dvAdditive d' of
	         Parsed v d'' ->
	           case dvSymbol d'' of
	             Parsed ')' d''' -> Parsed v d'''
	             _ -> alt2
		 _ -> alt2
	     _ -> alt2

    -- Primary <- Decimal
    alt2 = case dvDecimal d of
	     Parsed v d' -> Parsed v d'
	     NoParse -> NoParse

-- Parse a decimal number followed by optional whitespace
pDecimal :: Derivs -> Result Int
pDecimal d = case dvDigits d of
	       Parsed (v,n) d' ->
		 case dvWhitespace d' of
		   Parsed _ d'' -> Parsed v d''
		   _ -> NoParse
	       _ -> NoParse

-- Parse a string of consecutive decimal digits
pDigits :: Derivs -> Result (Int, Int)
pDigits d = case dvDigit d of
	      Parsed vl d' ->
	        case dvDigits d' of
		  Parsed (vr, n) d'' ->
		    Parsed (vl*10^n+vr, n+1) d''
		  _ -> Parsed (vl, 1) d'
	      _ -> NoParse

-- Parse a decimal digit
pDigit :: Derivs -> Result Int
pDigit d = case dvChar d of
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

-- Parse a symbol character followed by optional whitespace
pSymbol :: Derivs -> Result Char
pSymbol d = case dvChar d of
		Parsed c d' ->
		  if c `elem` "+-*/%()"
		  then case dvWhitespace d' of
			 Parsed _ d'' -> Parsed c d''
			 _ -> NoParse
		  else NoParse
		_ -> NoParse

-- Parse zero or more whitespace characters
pWhitespace :: Derivs -> Result ()
pWhitespace d = case dvChar d of
		  Parsed c d' ->
		    if isSpace c
		    then pWhitespace d'
		    else Parsed () d
		  _ -> Parsed () d

