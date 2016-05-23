-- Simple data type to keep track of character positions
-- within a text file or other text stream.
module Pos where


data Pos = Pos {
		posFile	:: !String,
		posLine	:: !Int,
		posCol	:: !Int
	}

nextPos (Pos file line col) c =
	if c == '\n' then Pos file (line + 1) 1
	else if c == '\t' then Pos file line ((div (col + 8 - 1) 8) * 8 + 1)
	else Pos file line (col + 1)

instance Eq Pos where
	Pos f1 l1 c1 == Pos f2 l2 c2 =
		f1 == f2 && l1 == l2 && c1 == c2

instance Ord Pos where
	Pos f1 l1 c1 <= Pos f2 l2 c2 =
		(l1 < l2) || (l1 == l2 && c1 <= c2)

instance Show Pos where
	show (Pos file line col) = file ++ ":" ++ show line ++ ":" ++ show col


showPosRel (Pos file line col) (Pos file' line' col') =
	if (file == file')
	then	if (line == line')
		then "column " ++ show col'
		else "line " ++ show line' ++ ", column " ++ show col'
	else show (Pos file' line' col')


