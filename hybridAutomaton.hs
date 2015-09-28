module HybridAutomaton where
import Data.List
import Data.Maybe
import Data.Text as T hiding (map,head,split,foldr,filter,tail)

-- VarH represents a variable of the HA which can have a inicial value, represented as a singleton list of Float, and it also has another singleton list which represents extra condition about the variable
newtype VarH     = VarH       { unVarH     :: ((String, [Float]), [String]) } deriving (Eq, Show, Ord)
-- Loc is a Node of the HA
newtype Loc      = Loc        { unLoc :: String } deriving (Eq, Show)
-- Edge is self-explanatory ((source,target),(condition,assignments))
newtype Edge = Edge { unEdge :: ((Loc,Loc), (String, String)) } deriving (Eq, Show)
-- Invariant of a Loc
newtype Inv = Inv { unInv :: (Loc,String) } deriving (Eq, Show)
-- Differential Equation for a Loc 
newtype Flow = Flow { unFlow :: (Loc,String) } deriving (Eq, Show)


-- Map between [Loc] and [Int]
data HybridAutomaton = HA { 
                            nameH       :: String,
                            varsH       :: [VarH],
                            locs        :: [Loc],
                            edges       :: [Edge],
                            invs        :: [Inv],
                            flows       :: [Flow],
                            initH       :: Loc,
                            initCond    :: String,
                            toProve     :: String,
                            cycleInv    :: String
                          } deriving (Show, Eq)

replaceTRUEAndFALSE :: String -> String
replaceTRUEAndFALSE s = let target1 = pack "TRUE"
                            target2 = pack "FALSE"
                            replacement1 = pack "1"
                            replacement2 = pack "0"
                            toReplace = pack s
                            final = ((replace target1 replacement1) . (replace target2 replacement2)) toReplace
                        in unpack final
                        

keymaeraPrint :: HybridAutomaton -> String
keymaeraPrint a = let
                      locals = locs a
                      initState = initH a
                      initStateVal = (read::String -> Float) $ show (fromJust $ (elemIndex initState locals))
                      state = VarH (("st",[initStateVal]),[])
                      vars = varsH a ++ [state]
                      inCod = initCond a
                      cyInv = cycleInv a
                      tProve = toProve a
                  in
                    "\\functions{\n" ++
                    "\tR deltaCycleMin;\n\tR deltaCycleMax;\n" ++
                    "}\n" ++
                    "\\programVariables {" ++ 
                    keymaeraPrintVar vars ++
                    "\n}\n" ++
                    "/**\n" ++
                    keymaeraPrintCommentLoc locals locals ++
                    "**/\n" ++
                    "\\problem {\n" ++
                    "\t(\n" ++
                    "\t\t(\n" ++
                    "\t\t\t" ++ inCod ++ "&\n" ++
                    "\t\t\tdeltaCycleMin>0 & deltaCycleMax>0 &\n" ++
                    "\t\t\tdeltaCycleMin < deltaCycleMax\n" ++
                    "\t\t)\n" ++
                    "\t\t->\n" ++
                    "\t\t\\[\n" ++
                    keymaeraPrintInitVar (filter ((/=[]).snd . fst . unVarH) vars) ++
                    "\t\t\\](\n" ++
                    "\t\t\t" ++ (keyMaeraPrintInitState initState locals) ++ "\n" ++
                    "\t\t\t->\n"++
                    "\t\t\t\\[\n" ++
                    "\t\t\t\t(\n" ++
                    keymaeraPrintLocs locals a ++
                    "\t\t\t\t)*@invariant(\n" ++
                    "\t\t\t\t             \t" ++ cyInv ++ "\n" ++
                    "\t\t\t\t            )\n" ++
                    "\t\t\t\\](\n" ++
                    "\t\t\t\t" ++ tProve ++ "\n" ++
                    "\t\t\t  )\n" ++
                    "\t\t)\n" ++
                    "\t)\n" ++
                    "}"

-- Loc tem de ser passado para inteiros 
{--keymaeraPrintLoc :: [Int] -> 
keymaeraPrintLoc [x] ... =
keymaeraPrintVar (x:xs) .. = 

keymaeraPrintEdge ...
--}

keymaeraPrintLocs :: [Loc] -> HybridAutomaton -> String
keymaeraPrintLocs [l] ha = keymaeraPrintLoc l ha
keymaeraPrintLocs (l:ls) ha = (keymaeraPrintLoc l ha) ++
                          "\n\t\t\t\t\t++\n" ++
                          (keymaeraPrintLocs ls ha)

keymaeraPrintLoc :: Loc -> HybridAutomaton -> String
keymaeraPrintLoc a ha = let aEdges   = edges ha
                            aLocs    = locs ha
                            aflows   = flows ha
                            ainvs    = invs ha
                            locNumb  = show (fromJust $ (elemIndex a aLocs))
                            locEdges = filter ((==a).fst. fst .unEdge) aEdges 
                            locFlow  = filter ((==a).fst.unFlow) aflows 
                            locInv   = filter ((==a).fst.unInv)  ainvs
                        in "\t\t\t\t\t(\n" ++
                           "\t\t\t\t\t\t?(st="++ locNumb ++");\n" ++
                           "\t\t\t\t\t\t\t(\n" ++
                           keymaeraPrintEdges locEdges ha++
                           "\t\t\t\t\t\t\t\t++\n" ++
                           keymaeraPrintFlowsWithInv (head locFlow) (head locInv) ++ 
                           "\t\t\t\t\t\t\t)\n" ++
                           "\t\t\t\t\t)"

keymaeraPrintFlowsWithInv :: Flow -> Inv -> String
keymaeraPrintFlowsWithInv f i = let diff = (snd .unFlow) f
                                    inv = (snd . unInv) i
                                in "\t\t\t\t\t\t\t\t{"++ diff ++ "," ++ inv ++"}\n"


keymaeraPrintEdges :: [Edge] -> HybridAutomaton -> String
keymaeraPrintEdges [] ha = "!!"
keymaeraPrintEdges [e] ha = keymaeraPrintEdge e ha
keymaeraPrintEdges (e:es) ha = keymaeraPrintEdge e ha ++
                               "\t\t\t\t\t\t\t\t++\n" ++
                               keymaeraPrintEdges es ha   

keymaeraPrintEdge :: Edge -> HybridAutomaton -> String
keymaeraPrintEdge e ha = let aLocs     = locs ha
                             dest      = (snd . fst . unEdge) e
                             destNumb  = show (fromJust $ (elemIndex dest aLocs))
                             action    = (snd . snd . unEdge) e
                             condition = (fst . snd . unEdge) e
                         in if condition /= "" then
                              "\t\t\t\t\t\t\t\t?(" ++ replaceTRUEAndFALSE condition ++ ");\n" ++
                              "\t\t\t\t\t\t\t\t\t(\n" ++ 
                              "\t\t\t\t\t\t\t\t\t\t" ++ replaceTRUEAndFALSE action ++ "\n" ++
                              "\t\t\t\t\t\t\t\t\t\tst:=" ++ destNumb ++ "\n" ++
                              "\t\t\t\t\t\t\t\t\t)" 
                            else
                              "\t\t\t\t\t\t\t\t(" ++
                              "\t\t\t\t\t\t\t\t\t\t" ++ replaceTRUEAndFALSE action ++ "\n" ++
                              "\t\t\t\t\t\t\t\t\t\tst:=" ++ destNumb ++ "\n" ++
                              "\t\t\t\t\t\t\t\t)" 
keyMaeraPrintInitState :: Loc -> [Loc] -> String
keyMaeraPrintInitState loc l' = "st=" ++ show (fromJust $ (elemIndex loc l'))

keymaeraPrintCommentLoc :: [Loc] -> [Loc] -> String
keymaeraPrintCommentLoc [] _ = ""
keymaeraPrintCommentLoc l l' = let loc = (head l)
                               in "\t" ++ (unLoc loc) ++ ":" ++ show (fromJust $ (elemIndex loc l')) ++ "\n" ++
                               keymaeraPrintCommentLoc (tail l) l'

keymaeraPrintVar :: [VarH] -> String
keymaeraPrintVar [] = ""
keymaeraPrintVar (x:xs) = "\n\tR " ++ (fst . fst . unVarH) x ++ ";" ++ keymaeraPrintVar xs 

keymaeraPrintInitVar :: [VarH] -> String
keymaeraPrintInitVar [x] = let nameVar = (fst . fst . unVarH) x
                               initValue = (head . snd . fst . unVarH) x
                              in 
                             "\t\t\t" ++ nameVar ++ ":=" ++ show initValue ++ "\n"
keymaeraPrintInitVar (x:xs) = let nameVar = (fst . fst . unVarH) x
                                  initValue = (head . snd . fst . unVarH) x
                              in 
                             "\t\t\t" ++ nameVar ++ ":=" ++ show initValue ++ ";\n" ++
                             keymaeraPrintInitVar xs