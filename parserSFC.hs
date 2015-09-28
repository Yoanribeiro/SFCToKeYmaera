{-# LANGUAGE Arrows, NoMonomorphismRestriction, OverloadedStrings #-}

module Main where

import Data.Text as T hiding (map,head) 
import Text.XML.HXT.Core
import System.Environment  --para uso do getArgs
import SfcHXT
import Sfc
import SfcHXTToSfc
import AnnotationParser
import HybridAutomaton
import SfcToHA
import Cp as Cp

atTag:: (ArrowXml a) => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

main:: IO()
main = do
       -- Dealing with SFC
       [src] <- getArgs
       sfc <- runX(readDocument [ withValidate no, withRemoveWS yes ] src  >>> getSFC)
       let sfcP = genSFC(head(sfc))
       --print sfcP
       --putStr "\n"
       
       -- Dealing with Annotations

       let annots = annotations sfcP
       let codeAnnots = Prelude.filter (\s -> isCODE s) annots
       let extraAnnots = Prelude.filter (\s -> isExtra s) annots
       let invariantAnnots = Prelude.filter (\s-> isInvariant s) annots
       let toProveAnnots = Prelude.filter (\s -> isToProve s) annots
       let initCondAnnots = Prelude.filter (\s -> isInitCond s) annots

       if (Prelude.length codeAnnots /= 0) then 
          case parsedCODES (head codeAnnots) of
            Left err -> putStr $ "CODE annotation error: " ++ (show err)
            Right (codes,codeVars) -> if (Prelude.length extraAnnots /= 0) then
                                        case parsedExtras (head extraAnnots) of 
                                          Left err2 -> putStr $ "Extra annotation error: " ++ (show err2)
                                          Right (extras, extrasVars) -> 
                                                                       if (Prelude.length initCondAnnots /= 0) then 
                                                                          case parsedInitCond (head initCondAnnots) of 
                                                                            Left err3 -> putStr $ "Initial condition annotation error: " ++ (show err3)
                                                                            Right ini -> if (Prelude.length invariantAnnots /= 0) then 
                                                                                          case parsedInv (head invariantAnnots) of 
                                                                                            Left err3 -> putStr $ "Invariant annotation error: " ++ (show err3)
                                                                                            Right inv -> if (Prelude.length toProveAnnots /= 0) then 
                                                                                                          case parsedToProve (head toProveAnnots) of 
                                                                                                            Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4)
                                                                                                            Right toProve -> do
                                                                                                                              let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                                              let hA = genHAFromSfc sfcP extraVarsToHA codes extras ini inv toProve
                                                                                                                              --putStr (show codes)
                                                                                                                              --print hA
                                                                                                                              writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                                                              putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                                                        else
                                                                                                          do
                                                                                                            let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                            let hA = genHAFromSfc sfcP extraVarsToHA codes extras ini inv ""
                                                                                                            --print hA
                                                                                                            writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA) 
                                                                                                            putStr "Warning: No expression to prove in the model\n"
                                                                                                            putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                                        else
                                                                                          do
                                                                                            if (Prelude.length toProveAnnots /= 0) then
                                                                                              case parsedToProve (head toProveAnnots) of 
                                                                                                Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4);
                                                                                                Right toProve -> do
                                                                                                                  let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                                  let hA = genHAFromSfc sfcP extraVarsToHA codes extras ini "" toProve;
                                                                                                                  ----print hA
                                                                                                                  writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA);
                                                                                                                  putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                                            else
                                                                                              do
                                                                                                let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                let hA = genHAFromSfc sfcP extraVarsToHA codes extras ini "" ""
                                                                                                writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                                putStr "Warning: No expression to prove in the model\n"
                                                                                                putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                                            putStr "Warning: No invariant added to the model\n"
                                                                      else
                                                                        do
                                                                          if (Prelude.length invariantAnnots /= 0) then 
                                                                            case parsedInv (head invariantAnnots) of 
                                                                              Left err3 -> putStr $ "Invariant annotation error: " ++ (show err3)
                                                                              Right inv -> if (Prelude.length toProveAnnots /= 0) then 
                                                                                            case parsedToProve (head toProveAnnots) of 
                                                                                              Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4)
                                                                                              Right toProve -> do
                                                                                                                let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                                let hA = genHAFromSfc sfcP extraVarsToHA codes extras "" inv toProve
                                                                                                                --print hA
                                                                                                                writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                          else
                                                                                            do
                                                                                              let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                              let hA = genHAFromSfc sfcP extraVarsToHA codes extras "" inv ""
                                                                                              --print hA
                                                                                              writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA) 
                                                                                              putStr "Warning: No expression to prove in the model\n"
                                                                                              putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                          else
                                                                            do
                                                                              if (Prelude.length toProveAnnots /= 0) then
                                                                                case parsedToProve (head toProveAnnots) of 
                                                                                  Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4);
                                                                                  Right toProve -> do
                                                                                                    let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                                    let hA = genHAFromSfc sfcP extraVarsToHA codes extras "" "" toProve;
                                                                                                    --print hA
                                                                                                    writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA);
                                                                                                    putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                              else
                                                                                do
                                                                                  let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                  let hA = genHAFromSfc sfcP extraVarsToHA codes extras "" "" ""
                                                                                  writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                  --print hA
                                                                                  putStr "Warning: No expression to prove in the model\n"
                                                                                  putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                              putStr "Warning: No invariant added to the model\n"
                                                                          putStr "Warning: No initial condition added to the model"
                                      else 
                                        if (Prelude.length initCondAnnots /= 0) then 
                                          case parsedInitCond (head initCondAnnots) of 
                                            Left err3 -> putStr $ "Initial condition annotation error: " ++ (show err3)
                                            Right ini -> if (Prelude.length invariantAnnots /= 0) then 
                                                          case parsedInv (head invariantAnnots) of 
                                                            Left err3 -> putStr $ "Invariant annotation error: " ++ (show err3)
                                                            Right inv -> if (Prelude.length toProveAnnots /= 0) then 
                                                                          case parsedToProve (head toProveAnnots) of 
                                                                            Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4)
                                                                            Right toProve -> do
                                                                                              let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                              let hA = genHAFromSfc sfcP extraVarsToHA codes [] ini inv toProve
                                                                                              --print hA
                                                                                              writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                              putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                                        else
                                                                          do
                                                                            let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                            let hA = genHAFromSfc sfcP extraVarsToHA codes [] ini inv ""
                                                                            print hA
                                                                            writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA) 
                                                                            putStr "Warning: No expression to prove in the model\n"
                                                                            putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                        else
                                                          do
                                                            if (Prelude.length toProveAnnots /= 0) then
                                                              case parsedToProve (head toProveAnnots) of 
                                                                Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4);
                                                                Right toProve -> do
                                                                                  let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                  let hA = genHAFromSfc sfcP extraVarsToHA codes [] ini "" toProve;
                                                                                  --print hA
                                                                                  writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA);
                                                                                  putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                            else
                                                              do
                                                                let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                let hA = genHAFromSfc sfcP extraVarsToHA codes [] ini "" ""
                                                                writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                --print hA
                                                                putStr "Warning: No expression to prove in the model\n"
                                                                putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                            putStr "Warning: No invariant added to the model\n"
                                      else
                                        do
                                          if (Prelude.length invariantAnnots /= 0) then 
                                            case parsedInv (head invariantAnnots) of 
                                              Left err3 -> putStr $ "Invariant annotation error: " ++ (show err3)
                                              Right inv -> if (Prelude.length toProveAnnots /= 0) then 
                                                            case parsedToProve (head toProveAnnots) of 
                                                              Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4)
                                                              Right toProve -> do
                                                                                let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                                let hA = genHAFromSfc sfcP extraVarsToHA codes [] "" inv toProve
                                                                                --print hA
                                                                                writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                                                putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                                          else
                                                            do
                                                              let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                              let hA = genHAFromSfc sfcP extraVarsToHA codes [] "" inv ""
                                                              --print hA
                                                              writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA) 
                                                              putStr "Warning: No expression to prove in the model\n"
                                                              putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                          else
                                            do
                                              if (Prelude.length toProveAnnots /= 0) then
                                                case parsedToProve (head toProveAnnots) of 
                                                  Left err4 -> putStr $ "To Prove annotation error: " ++ (show err4);
                                                  Right toProve -> do
                                                                    let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                                    let hA = genHAFromSfc sfcP extraVarsToHA codes [] "" "" toProve;
                                                                    --print hA
                                                                    writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA);
                                                                    putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                              else
                                                do
                                                  let extraVarsToHA = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (codeVars)
                                                  let hA = genHAFromSfc sfcP extraVarsToHA codes [] "" "" ""
                                                  writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)
                                                  --print hA
                                                  putStr "Warning: No expression to prove in the model\n"
                                                  putStr ("Success: Generated .key model with name: " ++ (nameH hA) ++ "\n")
                                              putStr "Warning: No invariant added to the model\n"
                                          putStr "Warning: No initial condition added to the model"
       else 
          putStr "Error: There is no Differential Equation to be added to the model\n"
       --print codeAnnots
       --print extraAnnots
       --print invariantAnnots
       --print toProveAnnots 
       

       --let extraVars = map (Var . (Cp.split (Cp.split id (const [])) (const []) ) ) (getVars (fst annots))
       --print extraVars
       --let inv = (fst.snd) annot
       --let toProve = (snd.snd) annot
       --let codes = (getCodes.fst.fst) annot
       --let toReplace = (getExtras.snd.fst) annot
       --let hA = genHAFromSfc sfcP extraVars codes toReplace inv toProve
       --writeFile ((nameH hA) ++ ".key") (keymaeraPrint hA)




getInputVarPs :: (ArrowXml a) => a XmlTree VarP
getInputVarPs = atTag "inputVars" >>> atTag "variable" >>>
    proc l -> do  
        nameVarP <- getAttrValue "name" -< l
        typeV <- atTag "type" >>> getChildren >>> getName -< l
        value <- getChildren >>> ifA( hasName "initialValue") (atTag "simpleValue" >>> getAttrValue "value" >>> arr ((: []))) (constA []) -< l
        returnA -< VarP (typeV, (nameVarP, (value, "inputVar") ) )

getOutputVarPs :: (ArrowXml a) => a XmlTree VarP
getOutputVarPs = atTag "outputVars" >>> atTag "variable" >>>
    proc l -> do  
        nameVarP <- getAttrValue "name" -< l
        typeV <- atTag "type" >>> getChildren >>> getName -< l
        value <- getChildren >>> ifA( hasName "initialValue") (atTag "simpleValue" >>> getAttrValue "value" >>> arr ((: []))) (constA []) -< l
        returnA -< VarP (typeV, (nameVarP, (value, "outputVar") ) )

getLocalVarPs :: (ArrowXml a) => a XmlTree VarP
getLocalVarPs = atTag "localVars" >>> atTag "variable" >>>
    proc l -> do  
        nameVarP <- getAttrValue "name" -< l
        typeV <- atTag "type" >>> getChildren >>> getName -< l
        value <- ifA( deep (hasName "initialValue")) (atTag "simpleValue" >>> getAttrValue "value" >>> arr ((: []))) (constA []) -< l
        returnA -< VarP (typeV, (nameVarP, (value, "localVar") ) )

getStepPs :: (ArrowXml a) => a XmlTree BodyElem
getStepPs = atTag "SFC" >>> atTag "step" >>>
	proc l -> do
        nameStepP <- getAttrValue "name" -< l
        ide <- getAttrValue "localId" -< l
        connIn <- atTag "connection" >>> getAttrValue "refLocalId" -< l
        returnA -< BodyElem (read ide :: Int, (nameStepP, ( StepT, ([read connIn :: Int], ("null", "null" ) ) ) ) ) 


getInitStepP :: (ArrowXml a) => a XmlTree BodyElem
getInitStepP = atTag "SFC" >>> atTag "step" >>> filterA ( hasAttrValue "initialStep" (== "true") ) >>>
    proc l -> do
    	nameStepP <-  (getAttrValue "name") -< l
        ide <- getAttrValue "localId" -< l
        connIn <- atTag "connection" >>> getAttrValue "refLocalId" -< l
        returnA -< BodyElem (read ide :: Int, (nameStepP, ( StepT, ([read connIn :: Int], ("null", "null" ) ) ) ) ) 



getActionPs :: (ArrowXml a) => a XmlTree BodyElem
getActionPs = atTag "SFC" >>> atTag "actionBlock" >>>
    proc l -> do
        ide <- getAttrValue "localId" -< l
        connIn <- atTag "connection" >>> getAttrValue "refLocalId" -< l
        act <- atTag "action" -< l   
        qual <- getAttrValue "qualifier" -< act
        action <-  atTag "xhtml:p" >>> getChildren >>> getText >>> (arr T.pack) >>> (arr T.strip) >>> (arr T.unpack) -< act
        returnA -< BodyElem (read ide :: Int, ("null", ( ActionBlock, ([read connIn :: Int], (qual, action ) ) ) ) ) 

getTransitionPs :: (ArrowXml a) => a XmlTree BodyElem
getTransitionPs = atTag "SFC" >>> atTag "transition" >>>
    proc l -> do
		ide <- getAttrValue "localId" -< l
		connIn <- atTag "connection" >>> getAttrValue "refLocalId" -< l
		cond <- atTag "xhtml:p" >>> getChildren >>> getText >>> (arr T.pack) >>> (arr T.strip) >>> (arr T.unpack) -< l
		returnA -< BodyElem (read ide :: Int, ("null", ( TransitionT, ([read connIn :: Int], ("null", cond ) ) ) ) ) 

getDivergence :: (ArrowXml a) => a XmlTree BodyElem
getDivergence = atTag "SFC" >>> atTag "selectionDivergence" >>>
    proc l -> do
    	ide <- getAttrValue "localId" -< l
    	connIn <- atTag "connection" >>> getAttrValue "refLocalId" -< l
    	returnA -< BodyElem (read ide :: Int, ("null", ( SelectionDivergence, ([read connIn :: Int], ("null", "null" ) ) ) ) )

getConvergence :: (ArrowXml a) => a XmlTree BodyElem
getConvergence = atTag "SFC" >>> atTag "selectionConvergence" >>>
    proc l -> do
    	ide <- getAttrValue "localId" -< l
    	connIn <- listA allConnIn -< l
    	returnA -< BodyElem (read ide :: Int, ("null", ( SelectionConvergence, ( (map (read ::String -> Int) connIn, ("null", "null" ) ) ) ) ) )
    	where allConnIn =
    		atTag "connection" >>> getAttrValue "refLocalId"

getAnnotations :: (ArrowXml a) => a XmlTree String
getAnnotations = atTag "SFC" >>> atTag "comment" >>>
    proc l -> do
        ano <- atTag "xhtml:p" >>> getChildren >>> getText >>> (arr T.pack) >>> (arr T.strip) >>> (arr T.unpack) -< l
        returnA -< ano

getSFC :: (ArrowXml a) => a XmlTree SFCP 
getSFC = 
    atTag "pou" >>>
    filterA hasSFC >>>
    proc plcXML -> do
    na <- getAttrValue "name" -< plcXML
    inv   <- listA getInputVarPs -< plcXML
    outv  <- listA getOutputVarPs -< plcXML
    locv  <- listA getLocalVarPs -< plcXML
    st  <- listA getStepPs -< plcXML
    ini <- getInitStepP -< plcXML
    act <- listA getActionPs -< plcXML
    trans <- listA getTransitionPs -< plcXML
    dive <- listA getDivergence -< plcXML
    conv <- listA getConvergence -< plcXML
    ano <- listA getAnnotations -< plcXML
    returnA -< SFCP { nameP =na, varsP = inv ++ outv ++ locv , stepsP = st, actionsP = act, initStepP = ini, transitionsP = trans ++ dive ++ conv , annotationsP = ano, histP = True }
    where hasSFC =  
                getChildren      -- that have children
                >>> getChildren      -- that have children
                >>> hasName "SFC"