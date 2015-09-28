{-# LANGUAGE OverloadedStrings #-}
module SfcToHA where
import Sfc
import HybridAutomaton
import Cp
import AnnotationParser
import Data.Text as T hiding (map,head,split,foldr,filter) 
import qualified Data.Set as Set
import Data.List hiding (map,head,split,foldr,filter) 
import Data.Maybe



genHAFromSfc :: SFC -> [Var] -> [(NameStep, [(String,String)])] -> [(String,String)] -> String -> String -> String -> HybridAutomaton
genHAFromSfc sfc varsExtra codes extras iniCod inv toPr = let codesT = map (Loc >< id) codes
                                                              haTrans1 = genHAFromSfc1 sfc varsExtra extras iniCod inv toPr 
                                                          in genHAFromSfc2 haTrans1 codesT

-- Second Part of the transformation

genHAFromSfc2 :: HybridAutomaton -> [(Loc,[(String,String)])] -> HybridAutomaton
genHAFromSfc2 ha mapLocCODES = let 
                                  nameF = nameH ha
                                  varsF = (Set.toList.Set.fromList) (varsH ha)
                                  locTrans1 = locs ha
                                  
                                  locsFromCodes = getLocFromCODES mapLocCODES
                                  locandSubLoc = unionLocs locTrans1 locsFromCodes
                                  codesWithAllLocs = unionCODES locTrans1 mapLocCODES
                                  locsF = getLocs locandSubLoc

                                  edgesTrans1 = edges ha
                                  edgesF = Prelude.concat (map (genEdgesFromLoc edgesTrans1 locandSubLoc) locandSubLoc)
                                  
                                  flowsF = genFlowFromCODES codesWithAllLocs
                                  invsF = genInvFromCODES codesWithAllLocs

                                  toProveF = toProve ha
                                  initF = initH ha
                                  cycleInvF = cycleInv ha
                                  inCond = initCond ha

                               in HA { nameH = nameF, varsH = varsF, locs = locsF, edges = edgesF, invs = invsF, flows = flowsF, initH = initF , initCond = inCond, toProve = toProveF, cycleInv =cycleInvF }

-- Unify the Loc which does not have any CODE with the one who has CODES
unionLocs :: [Loc] -> [(Loc,[Loc])] -> [(Loc,[Loc])]
unionLocs [] lst = lst
unionLocs (l:ls) lst = unionLocs ls (unionLoc l lst)

unionLoc :: Loc -> [(Loc,[Loc])] ->  [(Loc,[Loc])]
unionLoc l [] = [(l,[])]
unionLoc l ((l',locs):ls) = if l == l' then ((l,locs):ls) else ((l',locs):(unionLoc l ls))

unionCODES :: [Loc] -> [(Loc,[(String,String)])] -> [(Loc,[(String,String)])]
unionCODES [] lst = lst
unionCODES (l:ls) lst = unionCODES ls (unionCODESaux l lst)

unionCODESaux :: Loc -> [(Loc,[(String,String)])] ->  [(Loc,[(String,String)])]
unionCODESaux l [] = [(l,[])]
unionCODESaux l ((l',codes):ls) = if l==l' then ((l,codes):ls) else ((l',codes):(unionCODESaux l ls))

-- dyn : Loc -> CODES
getCODES :: Loc -> [(Loc,[(String,String)])] -> [(String,String)]
getCODES _ [] = []
getCODES l ((l',codes):codesRest) = if l == l' then codes else getCODES l codesRest   

getLocFromCODES :: [(Loc,[(String,String)])] -> [(Loc,[Loc])]
getLocFromCODES [] = []
getLocFromCODES ((l,codes):ls) = (l,[(Loc ((unLoc l) ++ show x))| x <- [0..((Prelude.length codes) - 1)]]):getLocFromCODES ls
-- Edges 2nd transformation

getLocs :: [(Loc,[Loc])] -> [Loc]
getLocs [] = []
getLocs ((l,ls):xs) = (l:ls) ++ (getLocs xs)

getSubLocs :: Loc -> [(Loc,[Loc])] -> [Loc]
getSubLocs l [] = []
getSubLocs l ((l',subl'):ls) = if l == l' then subl' else getSubLocs l ls 

genEdgesFromLoc :: [Edge] -> [(Loc,[Loc])] -> (Loc,[Loc]) -> [Edge]
genEdgesFromLoc edgesT1 remLocs (l,ls) = let 
                                           mainLoc = l
                                           sublocs = ls
                                           edgesWithSrcMainLoc = filter ((==mainLoc).fst.fst.unEdge) edgesT1
                                           edgesWithDstMainLoc = filter ((==mainLoc).snd.fst.unEdge) edgesWithSrcMainLoc
                                           edgesWithDstDiffMainLoc = filter ((/=mainLoc).snd.fst.unEdge) edgesWithSrcMainLoc
                                         in genEdges2 l ls remLocs edgesWithDstDiffMainLoc ++ genLoopLoc (l,ls) edgesWithDstMainLoc ++ genIdentityEdge (l,ls)

-- Arg1 Loc and subLocs,Arg2 remaining Locs, Arg3 Transitions of s to s'
genEdges2 :: Loc -> [Loc] -> [(Loc,[Loc])] -> [Edge] -> [Edge]
genEdges2 _ _ _ [] = []
genEdges2 l ls mapDest (e:es) = let dest = (snd.fst.unEdge) e
                                    dests = dest:(getSubLocs dest mapDest)
                                    srcs = (l:ls)
                                    cond = (fst.snd.unEdge) e
                                    assign = (snd.snd.unEdge) e
                                in genEdges2Aux srcs dests cond assign ++ genEdges2 l ls mapDest es

genEdges2Aux :: [Loc] -> [Loc] -> String -> String -> [Edge]
genEdges2Aux [] _ _ _  = []
genEdges2Aux (s:ss) dests cond assign = (map (aux s cond assign) dests) ++ genEdges2Aux ss dests cond assign
                                       where 
                                       aux src condi assigni dest = Edge ((src,dest),(condi,assigni)) 

-- Arg1 Loc in Dyn(s); Arg2 Edge of the loop from s to s'  
genLoopLoc :: (Loc,[Loc]) -> [Edge] -> [Edge]
genLoopLoc _ [] = []
genLoopLoc pair (e:es) = (aux pair e) ++ (genLoopLoc pair es)
                       where
                        aux (l,[]) edge = let cond = (fst.snd.unEdge) edge
                                              assign = (snd.snd.unEdge) edge
                                          in [Edge ((l,l),(cond,assign))]
                        aux (l,(l':ls')) edge = let cond = (fst.snd.unEdge) edge
                                                    assign = (snd.snd.unEdge) edge
                                                in (Edge ((l,l),(cond,assign))):(aux (l',ls') edge)
genIdentityEdge :: (Loc,[Loc]) -> [Edge]
genIdentityEdge (l,subLoc) = let locs = (l:subLoc)
                             in [Edge ((s,s'),("",""))| s <- locs, s' <- locs, s /= s'] 

-- Flow 2nd transformation
genFlowFromCODES :: [(Loc,[(String,String)])] -> [Flow]
genFlowFromCODES [] = []
genFlowFromCODES ((l,codes):rest) = let nameLoc = unLoc l
                                    in ([Flow (Loc (nameLoc ++ show num),((fst code) ++ ",t'=1")) | code <- codes, let num = (fromJust $ (elemIndex code codes)) ] ++ [Flow (l,"t'=1")]) ++ genFlowFromCODES rest
-- Invariants 2nd transformation

genInvFromCODES :: [(Loc,[(String,String)])] -> [Inv]
genInvFromCODES [] = []
genInvFromCODES ((l,codes):rest) = let nameLoc = unLoc l
                                   in ([Inv (Loc (nameLoc ++ show num),((f (snd code)) ++ "t<=deltaCycleMax" ++ (negOfOtherCond code codes) )) | code <- codes, let num = (fromJust $ (elemIndex code codes)) ] ++ [Inv (l,"t<=deltaCycleMax" ++ (negOfOtherCond ("null","null") codes))]) ++ genInvFromCODES rest
                                   where f "" = ""
                                         f s  = s ++ ","

negOfOtherCond :: (String,String) -> [(String,String)] -> String
negOfOtherCond _ [] = ""
negOfOtherCond code codes = let toNeg = Prelude.takeWhile (/=code) codes
                                cond  = foldr (\x acc -> if acc == "" then negCond x else negCond x ++ "," ++ acc ) "" (filter (\x -> x /= "") (map (snd) toNeg))
                            in if cond == ""
                               then ""
                               else "," ++ cond

-- First part of the transformation
genHAFromSfc1 :: SFC -> [Var] -> [(String,String)] -> String -> String -> String -> HybridAutomaton
genHAFromSfc1 sfc varsExtra extras iniCod inv toPr = let nm  = name sfc
                                                         vH  = map genVarHA ((vars sfc) ++ varsExtra) ++ [VarH (("t",[0]),[])] 
                                                         lH  = map genLoc (steps sfc)
                                                         fH  = map genFlow (lH)
                                                         iH  = map genInv (lH)
                                                         int = genLoc (initStep sfc)
                                                         trs = replaceCondTrans extras (transitions sfc)
                                                         edH = foldr (++) [] $ map (genEdgesFromStep trs (actions sfc)) (steps sfc)
                                                     in HA { nameH = nm, varsH = vH, locs = lH, edges = edH, invs = iH, flows = fH, initH = int, initCond = iniCod , toProve = toPr, cycleInv = inv}

replaceCondTrans :: [(String,String)] -> [Transition] -> [Transition]
replaceCondTrans rs [] = []
replaceCondTrans rs (t:ts) = (replaceCondTran rs t):(replaceCondTrans rs ts)

replaceCondTran :: [(String,String)] -> Transition -> Transition
replaceCondTran [] t = t
replaceCondTran ((tgt,rpl):ls) t = let target = (strip.pack) tgt
                                       replacement = pack rpl
                                       cond = (strip. pack . trim .getTransCond) t
                                       newCond = unpack $ (replace target replacement cond)
                                   in replaceCondTran ls ((Transition . ( id >< ((id >< id) >< (const newCond) ) ) . unTransition) t)
negCond :: String -> String
negCond "" = ""
negCond cond = "!("++ cond ++ ")" 

trim :: [Char] -> [Char]
trim [] = []
trim (x:xs) = if x == ' ' then trim xs else (x:trim xs)

genEdgesFromStep :: [Transition] -> [Action] -> Step -> [Edge]
genEdgesFromStep ts as s = let acts = filter ((==s). fst . snd . snd . unAction) as
                               trans = filter ((==s).fst.fst.snd.unTransition) ts
                               entryActions = filter ((=="P1").fst.snd.unAction) as
                               doActions = filter ((=="N").fst.snd.unAction) as 
                               exitActions = filter ((=="P0").fst.snd.unAction) as
	                          in genEdges trans entryActions doActions exitActions ++ [genLoopEdge s trans doActions]

genLoopEdge :: Step -> [Transition] -> [Action] -> Edge
genLoopEdge s ts doT = let acts = filter ((==s).fst.snd.snd.unAction) doT
                           auxTs = filter ((==s).fst.fst.snd.unTransition) ts
                           toAddActions = foldr (\x b -> if b == "" then x ++ b else x ++ ";" ++ b) "" (map (getAction) acts) 
                           src = genLoc s
                           dest = genLoc s
                           condsNeg = map (negCond.getTransCond) auxTs
                           cond = foldr (\x b -> if b == "" then x ++ b else x ++ "&" ++ b) "" condsNeg
                       in Edge ((src,dest),(cond,toAddActions))

genEdges :: [Transition] -> [Action] -> [Action] -> [Action] -> [Edge]
genEdges [] _ _ _ = []
genEdges (t:ts) entry doT exit = let src = (fst . fst .snd.unTransition) t 
                                     dest = (snd. fst . snd . unTransition) t 
                                     exitActionsSource = filter ( (==src).fst.snd.snd.unAction ) exit
                                     entryActionsDest = filter ((==dest).fst.snd.snd.unAction) entry
                                     doActionsDest = filter ((==dest).fst.snd.snd.unAction) doT
                                     cond = getTransCond t
                                     condsNeg = map (negCond.getTransCond) ts
                                 in (genEdge src dest cond condsNeg exitActionsSource entryActionsDest doActionsDest):(genEdges ts entry doT exit) 
                               
genEdge :: Step -> Step -> String -> [String] -> [Action] -> [Action] -> [Action] -> Edge
genEdge src dest cond condsNeg exit entry doT = let toAddCond = foldr (\x b -> if b == "" then x ++ b else x ++ "&" ++ b) "" condsNeg
                                                    toAddActions = foldr (\x b -> if b == "" then x ++ b else x ++ b) "" (map (getAction) (exit ++ entry ++ doT)) 
                                                    locSrc = genLoc src
                                                    locDest = genLoc dest
                                                    condRes = if toAddCond == [] then cond else cond ++ "&" ++ toAddCond
                                                in Edge ((locSrc,locDest),(condRes,toAddActions))    
       
genVarHA :: Var -> VarH
genVarHA = VarH . unVar 

genLoc :: Step -> Loc
genLoc = Loc . unStep

genFlow :: Loc -> Flow 
genFlow = Flow . (split id (const "t'=1"))

genInv :: Loc -> Inv
genInv = Inv . (split id (const "t<=deltaCycleMax"))


