--exports things necessary for running the machine as well as a sample machine
module Machines.Turing(State, Input, Direction, runMachine, incrimenter)
where

import Data.Maybe(fromJust)

data State = State Int | Final deriving Eq
data Input a = Input a | Blank | Marker deriving (Eq, Show)
data Direction = R | L deriving Eq
type Tape a = ([Input a], Input a, [Input a])
type Machine a = [((State, Input a), (State, Input a, Direction))]

moveTape :: Tape a -> Input a -> Direction -> Tape a
moveTape ([], x, ys) y L = ([], Blank, (y:ys))
moveTape (xs, x, []) y R = ((xs ++ [y]), Blank, [])
moveTape (xs, x, ys) y L = (init xs, last xs, (y:ys))
moveTape (xs, x, ys) y R = ((xs ++ [y]), head ys, tail ys)


transition :: Eq a => Machine a -> Tape a -> State -> (Maybe (Tape a))
transition machine tape Final = Just tape
transition machine tape@(xs, x, ys) state 	| rule == Nothing	= Nothing
											| otherwise			= transition machine (moveTape tape writeValue d) newState
	where 
		rule = lookup (state, x) machine
		(newState, writeValue, d) = fromJust rule

runMachine :: Eq a => Machine a -> [a] -> Maybe [a]
runMachine machine xs = fmap (concat . (map fromInput) . fromZipper) maybeTape
	where
		(y:ys) = map Input xs
		maybeTape = transition machine ([], y, ys) (State 0)
		fromZipper = \(xs, x, ys) -> xs ++ (x:ys)
		fromInput (Input x) = [x]
		fromInput symbol = []
		

incrimenter :: Machine Int
incrimenter = [ ((State 0, Input 0),	(State 0, 	Input 0, 	R)), 
				((State 0, Input 1),	(State 0, 	Input 1, 	R)),
				((State 0, Blank), 		(State 1, 	Blank, 		L)), 
				((State 1, Input 1),	(State 1, 	Input 0, 	L)),
				((State 1, Input 0),	(Final, 	Input 1, 	R)), 
				((State 1, Blank),		(Final, 	Input 1,	R))]

incriment :: String -> String
incriment s = let 	
				inputString = map (Input . read . (:[])) s :: [Input Int]
			  	tape = ([], head inputString, tail inputString)
			  in 	(show . fromJust) $ transition incrimenter tape (State 0)