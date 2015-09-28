module SfcHXT where


data TypeBodyElem = StepT | ActionBlock | TransitionT | SelectionConvergence | SelectionDivergence deriving (Eq, Show)

-- BodyElem (ID, (Type, ( [Reference(ConnIN)], (Qualifier, Text) ) ) )
newtype BodyElem 		    = BodyElem 	{ unBodyElem 		:: (Int, ( String, (TypeBodyElem, ([Int], (String, String) ) ) ) ) } deriving (Eq, Show)
-- VarP (type,(name, ([value], Category) ))
newtype VarP 		    = VarP 			{ unVarP 		:: (String,(String, ([String], String) )) } deriving (Eq, Show)



data SFCP = SFCP { 
						            nameP		     :: String,
						            varsP        :: [VarP],
                        stepsP       :: [BodyElem],
                        actionsP     :: [BodyElem],
                        initStepP    :: BodyElem,
                        transitionsP :: [BodyElem],
                        annotationsP :: [String],
                        histP		:: Bool
                      } deriving (Show, Eq)

findBodyElem :: Int -> [BodyElem] -> [BodyElem]
findBodyElem x []     = []
findBodyElem x (y:ys) = let idElemList = (fst . unBodyElem) y
                        in if x == idElemList then [y] else findBodyElem x ys

typeBodyElem :: BodyElem -> TypeBodyElem
typeBodyElem = fst . snd . snd . unBodyElem 

findOriginTransition :: BodyElem -> [BodyElem] -> [BodyElem]
findOriginTransition x []     = []
findOriginTransition x l = let idReference = (head . fst . snd . snd . snd . unBodyElem) x 
                               elemFound = (head (findBodyElem idReference l))
                           in case typeBodyElem elemFound of 
                                       StepT -> [elemFound]
                                       otherwise -> findOriginTransition elemFound l

findDestinationTransition :: BodyElem -> [BodyElem] -> [BodyElem]
findDestinationTransition x [] = []
findDestinationTransition x (y:ys)  = let idX = (fst . unBodyElem) x 
                                          idReferences = (fst . snd . snd . snd . unBodyElem) y
                                      in if idX `elem` idReferences then
                                             if typeBodyElem y == StepT then [y]
                                             else findDestinationTransition y ys
                                         else
                                             findDestinationTransition x (ys ++ [y])

findActionsStep :: BodyElem -> [BodyElem] -> BodyElem
findActionsStep x (y:ys) = let stepRef = (head . fst . snd . snd . snd . unBodyElem) x
                               actualStepID = (fst . unBodyElem) y
                           in if stepRef == actualStepID then y else findActionsStep x ys                                  