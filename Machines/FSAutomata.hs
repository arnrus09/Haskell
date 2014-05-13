--exports things necessary for running the machine as well as a sample machine
module Machines.FSAutomata(State, runMachine, oddNumOnes)
where

import Data.Maybe

newtype State = State Int deriving (Eq, Show)

type Starting_State = State
type Final_States = [State]
--a transition table contains a set of states si paired with a double which holds the 
--values f(si, 0) and f(si, 1) respectively where f is the transition function that
--maps a state and an input onto another state.
type Transition_Table = [(State, (State, State))]
type Machine = (Transition_Table, Starting_State, Final_States)

transition :: Final_States -> Transition_Table -> (State, [Int]) -> String
transition fs _ (s, [])			| elem s fs		= "recognized!"
								| otherwise		= "not recognized"
transition fs t (s, (c:xs))		| c==0	= transition fs t (fst $ fromJust $ lookup s t, xs)
								| c==1	= transition fs t (snd $ fromJust $ lookup s t, xs)

runMachine :: Machine -> [Int] -> String
runMachine m@(t, start, fs) xs = transition fs t (start, xs)


oddNumOnes :: Machine
oddNumOnes = (table, State 0, [State 1])
	where table = [(State 0, (State 0, State 1)), (State 1, (State 1, State 0))]

