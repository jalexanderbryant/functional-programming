module Chess where
rank_range = [1..8]
file_range = ['a', 'b', 'c', 'd','e', 'f', 'g', 'h']
-- See https://en.wikipedia.org/wiki/Chess for more details
-- Recall we only consider the situation where there is only a single
-- piece on the board

-- James Bryant 
-- Feb 10th, 2016
-- COP 4020 Programming Languages
-- Homework 1


-- see Rules - Set up for basic definitions

type File     = Char         -- column index
                             -- valid files are 'a','b',...,'h'
type Rank     = Int          -- row index
                             -- valid ranks are 1,2,...8
type Position = (File,Rank)   

data Color =
  Black | White
  deriving (Eq,Show)

data Piece =
  King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq,Show)

-- Function isLegalPosition: Determine if supplied position is valid
isLegalPosition :: Position -> Bool
-- Position is defined as a tuple (column, row)
isLegalPosition (x,y)
	| not (x `elem` file_range) = False
	| not (y `elem` rank_range) = False
	| otherwise = True
isLegalPosition _ = True 

isLegalMove :: Color -> Piece -> Position -> Position -> Bool
-- bad position
isLegalMove _ _ (s_file, s_rank) (e_file, e_rank)
      | (isLegalPosition (s_file, s_rank)) == False = False
      | (isLegalPosition (e_file, e_rank)) == False = False
      | (s_file, s_rank) == (e_file, e_rank) = False

isLegalMove _ Rook (s_file, s_rank) (e_file, e_rank) =
  ( (e_file, e_rank) `elem` (rook_file ++ rook_rank))
  where
        rook_file = [(a, b) |  a <- file_range, b <- rank_range, b == s_rank, a /= s_file ]
        rook_rank = [(c, d) |  c <- file_range, d <- rank_range, c == s_file, d /= s_rank ]

isLegalMove _ Bishop (s_file, s_rank) (e_file, e_rank) =
	((e_file, e_rank) `elem` (bish1 ++ bish2 ++ bish3 ++ bish4))
	where
		bish1 = zip [s_file..'h'] [s_rank, (s_rank-1)..1]
		bish2 = zip [s_file..'h'] [s_rank..8]
		bish3 = zip [s_file, (pred s_file)..'a'] [s_rank..8]
		bish4 = zip [s_file, (pred s_file)..'a'] [s_rank, (s_rank-1)..1]

isLegalMove _ Queen (s_file, s_rank) (e_file, e_rank) = 
	((e_file, e_rank) `elem` (vert++horz++queen1++queen2++queen3++queen4))
	where
		queen1 = zip [s_file..'h'] [s_rank, (s_rank-1)..1]
		queen2 = zip [s_file..'h'] [s_rank..8]
		queen3 = zip [s_file, (pred s_file)..'a'] [s_rank..8]
		queen4 = zip [s_file, (pred s_file)..'a'] [s_rank, (s_rank-1)..1]
		horz =  [ (a, b) | a <- file_range, b <- rank_range, b == s_rank, a/= s_file]
		vert =  [ (c, d) | c <- file_range, d <- rank_range, c == s_file, d /= s_rank]

isLegalMove _ King (s_file, s_rank) (e_file, e_rank) =
	((e_file, e_rank) `elem` (legal_king_pos) )
	where
        -- This is really....really...bad. 
        -- Hack
        -- TODO: 
        --  Simplify
		legal_king_pos = (succ s_file, s_rank ):(pred s_file, s_rank):(s_file, pred s_rank):(s_file, succ s_rank):(succ s_file, succ s_rank):(succ s_file, pred s_rank):(pred s_file, pred s_rank):(pred s_file, succ s_rank):[]

isLegalMove _ Knight (s_file, s_rank) (e_file, e_rank) = 
	((e_file, e_rank) `elem` (legal_knight_pos))
	where
        -- This is really....really...bad.
        -- Hack
        -- TODO: 
        --  Simplify
		legal_knight_pos = (succ (succ s_file), succ s_rank):(succ (succ s_file), pred s_rank):(pred (pred s_file), succ s_rank):(pred (pred s_file), pred s_rank):(pred s_file, succ(succ s_rank)):(pred s_file, pred (pred s_rank)):(succ s_file, pred (pred s_rank)):(succ s_file, succ (succ s_rank)):[]

isLegalMove White Pawn (s_file, s_rank) (e_file, e_rank)
	| (e_file == s_file) && (s_rank == 2) && (e_rank == 4) = True
	| (e_file == s_file) && (e_rank == (s_rank + 1)) = True
	| otherwise = False

isLegalMove Black Pawn (s_file, s_rank) (e_file, e_rank)
	| (e_file == s_file) && (s_rank == 7) && (e_rank == 5) = True
	| (e_file == s_file) && (e_rank == (s_rank-1)) = True
	| otherwise = False

isLegalMove Black King _ _ = True
isLegalMove _     _    _ _ = False
