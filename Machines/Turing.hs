module Machines.Turing(State(State,Final), Input(Input,Marker,Blank), Direction(L,R), runMachine)
where

import Data.Maybe(fromJust)
import Data.List(intersperse)
import Control.Monad.Writer

data Input = Input Int | Blank | Marker deriving Eq
data State = State Int | Final deriving (Eq, Show)
data Direction = R | L deriving Eq
newtype Tape = Tape ([Input], Input, [Input])
type Machine = [((State, Input), (State, Input, Direction))]

instance Show (Input) where
	show Blank 			= "B|"
	show Marker 		= "M|"
	show (Input other) 	= show other ++ "|"

instance Show (Tape) where
	show (Tape (xs, x, ys)) = showSide xs ++ showMiddle x ++ showSide ys
		where
			showSide s		= show =<< s
			showMiddle m	= "<|" ++ show m ++ ">|"

moveTape :: Tape -> Input -> Direction -> Tape
moveTape (Tape ([], x, ys)) y L = Tape ([], Blank, (y:ys))
moveTape (Tape (xs, x, [])) y R = Tape ((xs ++ [y]), Blank, [])
moveTape (Tape (xs, x, ys)) y L = Tape (init xs, last xs, (y:ys))
moveTape (Tape (xs, x, ys)) y R = Tape ((xs ++ [y]), head ys, tail ys)


transition ::  Machine -> Tape -> State ->  Writer [String] (Maybe Tape)
transition machine tape Final = do 
									tell ["halted :\t" ++ show tape]
									return (Just tape)
transition machine tape@(Tape (xs, x, ys)) state 	| rule == Nothing	= return Nothing
													| otherwise			= do
																			tell [show state ++ ":\t" ++ show tape]
																			transition machine newTape newState
	where 
		newTape						= moveTape tape writeValue d
		rule 						= lookup (state, x) machine
		(newState, writeValue, d)	= fromJust rule


runMachine :: Machine -> [Int] -> Writer [String] (Maybe Tape)
runMachine [] xs		= writer (Nothing, ["error"])
runMachine machine xs	= transition machine tape (State 0)
	where 
		(y:ys)	= map Input xs
		tape 	= (Tape ([], y, ys))



