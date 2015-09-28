module SfcHXTToSfc where
import Sfc
import SfcHXT

genSFC :: SFCP -> SFC
genSFC s = let nam = nameP s
               vs = map genVar (varsP s)
               st = stepsP s
               tr = transitionsP s
               actsAux = map (genAction st) (actionsP s)
               inSt = genStep (initStepP s)
               sts = map genStep st
               acts = ordActsForSteps sts actsAux
               aux = (filter ((== TransitionT) . fst . snd . snd . unBodyElem)) tr
               trsAux = map (genTransition (st ++ tr)) aux
               trs = ordTransForSteps sts trsAux
               anno = annotationsP s
          in SFC { name = nam, vars = vs, steps = sts, actions = acts,initStep = inSt, transitions = trs, hist = True, annotations = anno }

ordTransForSteps :: [Step] -> [Transition] -> [Transition]
ordTransForSteps [] _ = []
ordTransForSteps (s:ss) l = (ordTransForStep s l) ++ (ordTransForSteps ss l)

ordTransForStep :: Step -> [Transition] -> [Transition]
ordTransForStep s l = let transStep = filter ((==s).fst.fst.snd.unTransition) l
                      in ordTrans transStep 0


ordTrans :: [Transition] -> Int -> [Transition]
ordTrans [] _ = []
ordTrans (t:ts) n = let tr = t
                        bodyTr = (snd . unTransition) t
                    in (Transition (n,bodyTr)):(ordTrans ts (n+1))

ordActsForSteps :: [Step] -> [Action] -> [Action]
ordActsForSteps [] _ = []
ordActsForSteps (s:ss) l = (ordActsForStep s l) ++ (ordActsForSteps ss l)

ordActsForStep :: Step -> [Action] -> [Action]
ordActsForStep s l = let actStep = filter ((==s). fst . snd . snd . unAction) l
                     in ordActs actStep 0

ordActs :: [Action] -> Int -> [Action]
ordActs [] _ = []
ordActs (x:xs) n = let act = x
                       bodyAction = (snd . unAction) x
                   in (Action (n,bodyAction)):(ordActs xs (n+1))

genVar :: VarP -> Var
genVar x = let varType = (fst . unVarP) x
               name = (fst . snd . unVarP) x
               valuePossibleValue = (fst . snd . snd . unVarP) x
           in case varType of 
                "INT"  -> if length valuePossibleValue /= 0 then 
                	        Var ((name,[read (head valuePossibleValue) :: Float]),[]) 
                          else Var ((name,[]),[]) 
                "BOOL" -> if length valuePossibleValue /= 0 then 
                              if (head valuePossibleValue) == "TRUE" then
                        	      Var ((name,[1.0]),[(name ++ "= 0 |" ++ name ++ "= 1")]) 
                        	  else
                        	      Var ((name,[0.0]),[(name ++ "= 0 |" ++ name ++ "= 1")]) 
                        	  else 
                        	      Var ((name,[]),[(name ++ "= 0 |" ++ name ++ "= 1")]) 
                "REAL" -> if length valuePossibleValue /= 0 then 
                              Var ((name,[read (head valuePossibleValue) :: Float]),[]) 
                          else Var ((name,[]),[]) 
                otherwise -> undefined

genStep :: BodyElem -> Step
genStep x = let nameStep = (fst . snd . unBodyElem) x
             in Step nameStep

genAction :: [BodyElem] -> BodyElem -> Action
genAction l x = let idAction = (fst . unBodyElem) x
                    qualifier = (fst. snd . snd . snd . snd. unBodyElem) x 
                    text = (snd . snd . snd . snd . snd . unBodyElem) x
                    stepAction = genStep(findActionsStep x l) 
                in Action (idAction, (qualifier, (stepAction, text ) ) )

genTransition :: [BodyElem] -> BodyElem -> Transition
genTransition l x = let stepSrc = genStep(head(findOriginTransition x l))
                        stepDst = genStep(head(findDestinationTransition x l))
                        condition = (snd . snd . snd . snd . snd . unBodyElem) x
                    in Transition (0, ((stepSrc, stepDst), condition))
