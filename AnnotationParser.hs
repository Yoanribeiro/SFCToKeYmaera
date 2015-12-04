module AnnotationParser where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Expr
import Text.Parsec.Error
import Text.Parsec.Language 
import qualified Text.Parsec.Token    as P
import Data.Either
import Data.Set as S hiding (map, foldr, filter)
import Data.List hiding (filter)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)

-- Types and Grammmar

data Exp =
	  EVarDiff String
	  | EVar String  
	  | ENeg Exp
    | ENum Float
	  | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EExp Exp Exp
     deriving (Eq, Show)

data BExp =
     Formula Exp
     | EqFormula Exp Exp
     | LeFormula Exp Exp
     | LeqFormula Exp Exp
     | GeFormula Exp Exp
     | GeqFormula Exp Exp
     | NEqFormula Exp Exp
     | NotForm BExp
     | AndForm BExp BExp
     | OrForm BExp BExp
     | ImplForm BExp BExp
     | EquiForm BExp BExp
     | InParen BExp
      deriving (Eq, Show)

type NameStep = String

prettyprintExp :: Exp -> String
prettyprintExp (EVarDiff s) = s
prettyprintExp (EVar s)     = s
prettyprintExp (ENeg ex)    = "-" ++ prettyprintExp ex
prettyprintExp (ENum f)     = show f
prettyprintExp (EAdd e1 e2) = prettyprintExp e1 ++ "+" ++ prettyprintExp e2
prettyprintExp (ESub e1 e2) = prettyprintExp e1 ++ "-" ++ prettyprintExp e2
prettyprintExp (EMul e1 e2) = prettyprintExp e1 ++ "*" ++ prettyprintExp e2
prettyprintExp (EDiv e1 e2) = prettyprintExp e1 ++ "/" ++ prettyprintExp e2
prettyprintExp (EExp e1 e2) = prettyprintExp e1 ++ "^" ++ prettyprintExp e2

prettyprintBExp :: BExp -> String
prettyprintBExp (Formula e) = prettyprintExp e
prettyprintBExp (InParen e) = "("++ prettyprintBExp e ++ ")"
prettyprintBExp (EqFormula e1 e2) = prettyprintExp e1 ++ "=" ++ prettyprintExp e2
prettyprintBExp (NEqFormula e1 e2) = prettyprintExp e1 ++ "=" ++ prettyprintExp e2
prettyprintBExp (LeFormula e1 e2) = prettyprintExp e1 ++ "<" ++ prettyprintExp e2
prettyprintBExp (LeqFormula e1 e2) = prettyprintExp e1 ++ "<=" ++ prettyprintExp e2
prettyprintBExp (GeFormula e1 e2) = prettyprintExp e1 ++ ">" ++ prettyprintExp e2
prettyprintBExp (GeqFormula e1 e2) = prettyprintExp e1 ++ ">=" ++ prettyprintExp e2
prettyprintBExp (NotForm e) = "!" ++ prettyprintBExp e
prettyprintBExp (AndForm e1 e2) = prettyprintBExp e1 ++ "&" ++ prettyprintBExp e2
prettyprintBExp (OrForm e1 e2) = prettyprintBExp e1 ++ "|" ++ prettyprintBExp e2
prettyprintBExp (ImplForm e1 e2) = prettyprintBExp e1 ++ "->" ++ prettyprintBExp e2
prettyprintBExp (EquiForm e1 e2) = prettyprintBExp e1 ++ "<->" ++ prettyprintBExp e2


unEVar :: Exp -> String
unEVar (EVar e) = e

-- Auxilliar operators to combine the results from diferent parsers
(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

--lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle
                               { P.reservedOpNames = ["+", "-", "*", "/", "^","<=", "<","=", ">", ">=","!", "||", "&&", "!=", "->", "<->"]
                               }
                          )

--reservedOp :: String -> Parserc ()
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer

-- To parse Numbers

number :: Parsec String [Exp] String
number = many1 digit

plus :: Parsec String [Exp] String
plus = char '+' *> number

minus :: Parsec String [Exp] String
minus = char '-' <:> number

sinteger :: Parsec String [Exp] String
sinteger = plus <|> minus <|> number

sfloat :: Parsec String [Exp] Exp
sfloat = fmap (ENum . rd) $ sinteger <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> sinteger


nameStep :: Parsec String [Exp] NameStep
nameStep = (many1 $ alphaNum)
{--
pVarDiff :: Parsec String [Exp] Exp
pVarDiff = do
            res <- fmap EVarDiff $ (many1 $ alphaNum) <++> (many1 $ char '\'')
            modifyState ((res:) )
            return res
--}

pVarDiff :: Parsec String [Exp] Exp
pVarDiff = do
            res <- fmap EVarDiff $ ((fmap (unEVar) (pVar)) <++> (many1 $ char '\''));
            return res;

pVar :: Parsec String [Exp] Exp
pVar = do
        res <- fmap EVar $ (many1 $ alphaNum) 
        modifyState ((res:))
        return res

pExp :: Parsec String [Exp] Exp
pExp = buildExpressionParser opList pExpAtom
  where
    pExpAtom = (sfloat) <|> try (pVarDiff) <|> (pVar)  
    
    opList  = [ [ prefix "-" ENeg 
                , binary "^" (EExp) AssocLeft 
                ]
              , [ binary "*" (EMul) AssocLeft
                , binary "/" (EDiv) AssocLeft 
                ]
              , [ binary "+" (EAdd) AssocLeft 
                , binary "-" (ESub) AssocLeft 
                ]
              ]
pBExp :: Parsec String [Exp] BExp
pBExp = buildExpressionParser opList pBExpAtom
  where
    pBExpAtom = (pExp >>= 
                \e -> (   (fmap (EqFormula e)  $ reservedOp "="  *> pExp) 
                      <|> (fmap (LeFormula e)  $ reservedOp "<"  *> pExp)
                      <|> (fmap (LeqFormula e) $ reservedOp "<=" *> pExp)
                      <|> (fmap (GeFormula e)  $ reservedOp ">"  *> pExp)
                      <|> (fmap (GeqFormula e) $ reservedOp ">=" *> pExp)
                      <|> (fmap (NEqFormula e) $ reservedOp "!=" *> pExp)))
                <|>
                (do
                  string "(";
                  e <- pBExp
                  string ")";
                  return (InParen e)
                )
    
    opList  = [ [ prefix "!"  (NotForm) 
                , binary "&&" (AndForm) AssocLeft 
                ]
              , [ binary "||" (OrForm) AssocLeft
                , binary "->" (ImplForm) AssocLeft 
                ]
              , [ binary "<->" (EquiForm) AssocLeft 
                ]
              ]

odeParse :: Parsec String [Exp] String 
odeParse = do 
              whiteSpace
              e1 <- (fmap prettyprintExp $ pExp);
              spaces
              string "=";
              whiteSpace
              e2 <- (fmap prettyprintExp $ pExp); 
              return (e1 ++ "=" ++ e2)

pCond :: Parsec String [Exp] String
pCond = (fmap prettyprintBExp $ pBExp) <++> (fmap (foldr (++) "") $ many $ (string "," <++> pCond))  


pODES :: Parsec String [Exp] String
pODES = odeParse <++> (fmap (foldr (++) "") $ many $ (string "," <++> pODES)) 

pCODE :: Parsec String [Exp] (String,String)
pCODE = do
        first  <- pODES
        second <- option "" (string ":" *> pCond) 
        return (first,second)

pCODES :: Parsec String [Exp] [(String,String)]
pCODES = do
          headL <- pCODE
          tailL <- fmap (concat) (many (string ";" *> pCODES))
          return (headL:tailL) 

{--
 mainParsers
     - Conditional Ordinaral Differential Equation;
     - Expression to Prove
     - Expression for the Program Invariant
     - Extras: Expression which in SFC makes sense, such as a boolean sensor is equal to true but in HA it has to be represented in another way
--}


mainCODESParser :: Parsec String [Exp] ((NameStep, [(String,String)]),[String])
mainCODESParser =  do
                    char '@';
                    spaces;
                    step <- nameStep;
                    spaces;
                    string "->";
                    spaces;
                    codes <- pCODES;
                    vars <- fmap (fmap (prettyprintExp)) getState;
                    return ((step, codes), ((S.toList).(S.fromList))vars)

mainCODESParser1 :: Parsec String [Exp] ([(NameStep, [(String,String)])],[String])
mainCODESParser1 =  do
                    first <- (
                              do 
                                char '@';
                                spaces;
                                step <- nameStep;
                                spaces;
                                string "->";
                                spaces;
                                codes <- pCODES;
                                return (step, codes)
                             ) 
                    list <- many $ (
                                     do 
                                       char '\n'
                                       char '@';
                                       spaces;
                                       step <- nameStep;
                                       spaces;
                                       string "->";
                                       spaces;
                                       codes <- pCODES;
                                       return (step, codes)
                                   ) 
                    vars <- fmap (fmap (prettyprintExp)) getState;
                    return ( (first:list), ((S.toList).(S.fromList)) vars)
mainExtraCondParser :: Parsec String [Exp] ([(String,String)],[String])
mainExtraCondParser = do
                       string "@Extra:";
                       char '\n';
                       first <- (
                                  do
                                    e1 <- fmap (prettyprintBExp) pBExp 
                                    char ':'
                                    e2 <- fmap (prettyprintBExp) pBExp
                                    return (e1,e2)
                                )
                       list <- many $ (
                                        do
                                          char '\n'
                                          e1 <- fmap (prettyprintBExp) pBExp 
                                          char ':'
                                          e2 <- fmap (prettyprintBExp) pBExp
                                          return (e1,e2)
                                      )
                       vars <- fmap (fmap (prettyprintExp)) getState
                       return ((first:list),((S.toList).(S.fromList))vars)

mainInvParser :: Parsec String [Exp] String
mainInvParser = do
                  string "@invariant:"
                  inv <- fmap (prettyprintBExp) pBExp
                  return inv

mainToProveParser :: Parsec String [Exp] String
mainToProveParser = do
                      string "@toProve:"
                      toProve <- fmap (prettyprintBExp) pBExp
                      return toProve

mainInitCondParser :: Parsec String [Exp] String
mainInitCondParser = do
                      string "@initCond:"
                      cond <- fmap (prettyprintBExp) pBExp
                      return cond

parsedCODES :: String -> Either ParseError ([(NameStep, [(String,String)])],[String])
parsedCODES s = runParser mainCODESParser1 [] "" s

parsedExtras :: String -> Either ParseError ([(String,String)],[String])
parsedExtras s = runParser mainExtraCondParser [] "" s

parsedInv :: String -> Either ParseError String
parsedInv s = runParser mainInvParser [] "" s

parsedToProve :: String -> Either ParseError String
parsedToProve s = runParser mainToProveParser [] "" s

parsedInitCond :: String -> Either ParseError String
parsedInitCond s = runParser mainInitCondParser [] "" s

mainParserAnnotation :: [String] -> (([((NameStep, [(String,String)]),[String])],[([(String,String)],[String])]),(String,String))
mainParserAnnotation l = let codes = rights (map (runParser mainCODESParser [] "") l)
                             extras = rights (map (runParser mainExtraCondParser [] "") l)
                             inv = ((\d -> if d == [] then "" else (head d)). rights) (map (runParser mainInvParser [] "") l)
                             toProve = ((\d -> if d == [] then "" else (head d)) . rights) (map (runParser mainToProveParser [] "") l)
                         in ((codes, extras),(inv,toProve))

isExtra :: String -> Bool 
isExtra = isPrefixOf "@Extra"

isToProve :: String -> Bool
isToProve = isPrefixOf "@toProve"

isInvariant :: String -> Bool
isInvariant = isPrefixOf "@invariant"

isInitCond :: String -> Bool
isInitCond = isPrefixOf "@initCond"

isCODE :: String -> Bool 
isCODE = (\s -> not (isExtra s) && not (isToProve s) && not (isInvariant s) && not (isInitCond s)) 


getVars :: ([((NameStep, [(String,String)]),[String])],[([(String,String)],[String])]) -> [String]
getVars (f,s) = foldr (\f acc -> (snd f) ++ acc) [] f ++ foldr (\f acc -> (snd f) ++ acc) [] s

getExtras :: [ ( [(String,String)] , [String] ) ] -> [(String,String)]
getExtras = concat . (map fst)

getCodes :: [((NameStep, [(String,String)]),[String])] -> [(NameStep, [(String,String)])]
getCodes = map fst 
-- To parse every annotation map (parse mainParser "") (lines $ stringToParse) :: runParser mainCODESParser [] "" "@name->y'=1:y>=2::y'=1:y>=2"