-- Def in paper "A unifying semantics for sequential function charts"
module Sfc where
import Cp
-- Step is just the name of the step
newtype Step 		= Step 			{ unStep 		:: String } deriving (Eq, Show)
-- Variable has the name and a possible Value and conditions which comes from the type of the variable
newtype Var 		= Var 			{ unVar 		:: ((String, [Float]), [String]) } deriving (Eq, Show)
-- Action has a priority, a quantifier, the step in which it is associate with and the action in Structured Text
newtype Action 		= Action 		{ unAction 		:: (Int,(String, (Step, String))) } deriving (Eq, Show)
-- Transition has a priority, the inicial step and the final step, and the condition 
newtype Transition 	= Transition 	{ unTransition 	:: (Int,((Step,Step),String)) } deriving (Eq, Show)


data SFC = SFC { 
				    name		:: String,
					vars        :: [Var],
                    steps       :: [Step],
                    actions     :: [Action],
                    initStep    :: Step,
                    transitions :: [Transition],
                    hist		:: Bool,
                    annotations :: [String]
               } deriving (Show, Eq)

instance Ord Action where
 	compare = curry ( (uncurry compare) . ( (fst . unAction) >< (fst . unAction ) ) )

instance Ord Transition where
 	compare = curry ( (uncurry compare) . ( (fst . unTransition) >< (fst . unTransition ) ) )

getTransCond :: Transition -> String
getTransCond = snd . snd . unTransition

getAction :: Action -> String
getAction = snd . snd . snd . unAction


