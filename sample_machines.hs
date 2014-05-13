--simple command line demo with sample machines.  machines can be defind in the interpretter.

import Machines.Turing
import Control.Monad.Writer
import Control.Monad(sequence)

main = do
		putStrLn "\ntable shows command followed by\nthe type of machine it activates\n"
		loop

loop :: IO ()
loop = do
			putStrLn "\n--test machines--\ninc: incrimenter\n2n: 2 * n\n"
			putStrLn "machine:\t"
			choice <- getLine
			when (msg choice /= []) $ innerLoop (choose choice) (msg choice) 
			loop
		where 
			choose "2n" = twoN
			choose "inc"= incrimenter
			choose other= []
			msg "2n" = "unary-string:"
			msg "inc"= "bit-string:"
			msg other= []


innerLoop ::  [((State, Input), (State, Input, Direction))] -> String -> IO ()
innerLoop machine msg = do
							putStrLn $ msg 
							input <- getLine
							putStrLn "\n"
							sequence $ map putStrLn $ snd . runWriter $ runMachine machine $ map (read . return) input
							putStrLn "\n"
							loop
	

incrimenter :: [((State, Input), (State, Input, Direction))]
incrimenter = [	((State 0, Input 0),	(State 0, 	Input 0, 	R)), 
				((State 0, Input 1),	(State 0, 	Input 1, 	R)),
				((State 0, Blank), 		(State 1, 	Blank, 		L)), 
				((State 1, Input 1),	(State 1, 	Input 0, 	L)),
				((State 1, Input 0),	(Final, 	Input 1, 	R)), 
				((State 1, Blank),		(Final, 	Input 1,	R))	]

twoN :: [((State, Input), (State, Input, Direction))]
twoN = [	((State 0, Input 1), 		(State 1, 	Input 1, 	R)),
			((State 1, Input 1), 		(State 2, 	Marker, 	L)),
			((State 2, Input 1), 		(State 2, 	Input 1, 	L)),
			((State 2, Blank), 			(State 3, 	Input 1, 	R)),
			((State 3, Input 1), 		(State 3, 	Input 1, 	R)),
			((State 3, Marker), 		(State 1, 	Input 1, 	R)),
			((State 1, Blank), 			(Final, 	Blank,	 	R))	]

