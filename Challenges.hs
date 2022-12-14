-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return a randome value that is usually wrong 
-- Author of solutions written by Sohaib Karous
-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (alphaNorm, countAllReds, printLambda, parseLet, letToLambda,
    LamExpr(LamApp, LamAbs, LamVar), LetExpr(LetApp, LetDef, LetFun, LetVar, LetNum),
    lambdaToLet) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Applicative
import Data.List
import Data.Function

-- abstract data type for simple lambda calculus expressions
data LamExpr = LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int deriving (Show, Eq)

-- abstract data type for simple let expressions
data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
-- END OF CODE YOU MUST NOT MODIFY

-- Notes
-- Define functions
-- Apply functions to them

-- Functions taken from the paper : sepby1
-- Functions taken from slids : subst, free
-- Worked on solutions with Ben Thompson, Josh Hook, Wojciech Różowski. Although all implementation were my own
-- ADD YOUR OWN CODE HERE


-- Challenge 1
-- generate the alpha normal form for a simple lambda calculus expression
-- each bound variable is chosen to be the first one possible

-- Discussed with Ben Thompson and Josh Hook
alphaNorm :: LamExpr -> LamExpr 

-- Process first renames all of the non bounds to -1, names the bounds to their respective value and then finally re renames the nonbounds to their respective value
alphaNorm expr = finalRenameNonBound (renameBound (renameNonBound expr) []) (getFreeList expr)


-- Goes through the expression and renames all the -1 to the smallest value it can without clashing
finalRenameNonBound (LamVar x) _ = LamVar x
finalRenameNonBound (LamAbs x y) notAllowed  | x == -1 = LamAbs (getMax notAllowed) (finalRenameNonBound y notAllowed)
                                     | otherwise = LamAbs x (finalRenameNonBound y (x:notAllowed))                                         
finalRenameNonBound (LamApp x y) notAllowed = LamApp (finalRenameNonBound x ([c | c <- (getFreeList x), c `elem` notAllowed ])) (finalRenameNonBound y ([c | c <- (getFreeList y), c `elem` notAllowed ]))

getMax xs = head ([x | x <- [0..], ((x `elem` xs) == False)])
                              
--Goes through the expression and renames all bound to the smallest value it can
renameBound :: LamExpr -> [(Int,Int)] -> LamExpr
renameBound (LamVar x) xs | elem x [a | (a,y) <- xs] = LamVar (head [y | (a,y) <- xs, a==x])
                     | otherwise = (LamVar x)                                    
renameBound (LamApp x y) xs = LamApp (renameBound x ([(d,e) | (d,e) <- xs, bruh <- (getFreeList x) ,d == bruh])) (renameBound y ([(d,e) | (d,e) <- xs, bruh <- (getFreeList y) ,d == bruh])) 
renameBound (LamAbs x y) xs | elem x [a | (a,b) <- xs] = (LamAbs (head [b | (a,b) <- xs, a==x]) (renameBound y xs))
                      | x /= (-1) = (LamAbs (getFresh xs) (renameBound y ((x,(getFresh xs)):xs) ))
                      | otherwise = (LamAbs x (renameBound y xs))

-- Goes through the expression and renames all non bound to -1                     
renameNonBound :: LamExpr -> LamExpr                 
renameNonBound (LamVar x) = LamVar x 
renameNonBound (LamAbs x y) | x `elem` (getFreeList y) = (LamAbs x (renameNonBound y))
                      | otherwise =  (LamAbs (-1) (renameNonBound y))                     
renameNonBound (LamApp x y) = LamApp (renameNonBound x) (renameNonBound y)

                       
                    
-- Obtained from lecture slides
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x==y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x/= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

getFresh xs | (length xs) == 0 = 0
             | otherwise = (maximum $ [y | (a,y) <- xs]) +1

getAllVariable (LamAbs x y) = (getAllVariable y)
getAllVariable (LamApp x y) = (getAllVariable x) ++ (getAllVariable y)
getAllVariable (LamVar x) = [x]


getFreeList expr = [x | x <- (getAllVariable expr),(free x expr)]

-- Challenge 2
-- count all reduction paths for a given lambda expression m, of length up to a given limit l


-- Counts the amount of redexes
getRedex (LamApp (LamAbs a b) expr) = 1 + (getRedex expr)
getRedex (LamApp a b) = (getRedex a) + (getRedex b)
getRedex (LamAbs a b) = (getRedex b)
getRedex (LamVar a) = 0

-- Solution discussed with Wojciech Różowski and Ben Thompson


countAllReds :: LamExpr -> Int -> Int
countAllReds _ 0 = 0
countAllReds expr max | getRedex expr == 0 = 1 
                      | otherwise = sum [(countAllReds x (max-1))  | x <- (expressionListBuilder expr [1..getRedex expr])]  

-- Builds the results from the indexing                  
expressionListBuilder expression [] = [] 
expressionListBuilder expression (n:xs) = [nthReduction expression n] ++ expressionListBuilder expression xs                     



-- Iterates through the tree indexing the respective banches as it goes through
nthReduction (LamApp (LamAbs x m) n) i | i == 1 = subst m x n
                                       | otherwise = (LamApp (LamAbs x m) (nthReduction n (i - (getRedex m) - 1)))                                             
nthReduction (LamApp m n) i | i <= getRedex m = (LamApp (nthReduction m i) n)
                            | otherwise = (LamApp m (nthReduction n (i - getRedex m)))

                            
nthReduction (LamAbs m n) i = (LamAbs m (nthReduction n i))
-- Base case
nthReduction (LamVar x) i = LamVar x
                      

-- Obtained from slides
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
 x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
 x /=y && (free x e) = let x' = (rename x e1) in
 subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

rename x e1 = maximum (x : (getAllTaken e1)) + 1 

getAllTaken (LamVar x) = [x]
getAllTaken (LamAbs x e) = [x] ++ (getAllTaken (e))
getAllTaken (LamApp f g) = (getAllTaken f) ++ (getAllTaken g)


-- Challenge 3 
-- pretty print a lambda expression, combining abstraction variables
-- also recognising Scott numerals and printing these as numbers
-- finalising omitting brackets where possible and safe to do so

-- Each time it enters printLambda it checks if there exists a scott encoding
printLambda :: LamExpr -> String
printLambda xl | (scottEncoding xl 0) /= (-1) = show (scottEncoding xl 0)
               | otherwise = solve (xl) 
 
-- \x -> \y -> x = 0
-- \x -> \y -> y 
-- \x1->\x1->x1 \x1->\x2->x1"
--solve (LamApp (LamVar a) b) = "x"++(show a) ++ printLambda b

-- Pattern matching the respective lamexpr so we know where to bracket
solve (LamApp (LamApp a b) (LamApp c d)) = printLambda (LamApp a b) ++ " (" ++ printLambda (LamApp c d) ++")"
solve (LamApp (LamApp a b) (LamVar c)) = printLambda (LamApp a b) ++ " " ++ "x"++(show c)
solve (LamApp (LamVar a) (LamApp b c)) = "x" ++ (show a) ++ " (" ++ printLambda (LamApp b c) ++ ")"

-- If it contains a scott encoding then it does not bracket the expression
solve (LamApp (LamAbs a b) (LamApp c d)) | scottEncoding (LamAbs a b) 0 /= -1 = printLambda (LamAbs a b) ++ " " ++ "(" ++ printLambda (LamApp c d) ++ ")"
                                         | otherwise = "(" ++ printLambda (LamAbs a b) ++ ") " ++ "(" ++ printLambda (LamApp c d) ++ ")"
solve (LamApp (LamAbs a b) c) | scottEncoding (LamAbs a b) 0 /= (-1) = printLambda (LamAbs a b) ++ " " ++ printLambda c
                              | otherwise = "(" ++ printLambda (LamAbs a b) ++ ") " ++ printLambda c
solve (LamApp a b) = printLambda a ++ " " ++ printLambda b
solve (LamAbs a b) = "\\x" ++ (show a) ++ " -> " ++ printLambda b 
solve (LamVar a) = "x"++(show a)



-- Checks for the scott encoding at each step of the printing
scottEncoding :: LamExpr -> Int -> Int
scottEncoding (LamAbs x (LamAbs y (LamVar z))) counter | x == z = counter
                                                       | otherwise = -1

-- If it does not exist it will return a -1                                                      
scottEncoding (LamAbs x (LamAbs y (LamApp (LamVar z) expr))) counter | y == z && (scottEncoding expr 0) /= -1 = (scottEncoding expr counter+1)
                                                                     | otherwise = -1 
                                                                      
                                                                                                                                       
scottEncoding _ _ = -1
                                              
-- Challenge 4
-- parse recursive let expression, possibly containing numerals

-- Discussed with Ben Thompson and Wojciech Różowski
parseLet :: String -> Maybe LetExpr

-- data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)
parseLet line | length check == 0 = Nothing
              | right == "" = Just parsed
              | otherwise = parseLet right
              where
              check = parse rest line             
              [(parsed,right)] = parse rest line

-- Obtained from paper  

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do 
    a  <- p
    as <- many (do {sep; p})
    return (a:as) 
    
-- Looks for syntactically correct functions and returns the number
func :: Parser Int
func = do space
          char 'f'
          i <- int
          return i

-- Looks for syntactically correct variables and returns the number
var :: Parser Int
var = do space 
         char 'x'
         i <- int
         return i
         
         
         
function :: Parser Int
function = token func

variable :: Parser Int
variable = token var 
 
          

         
-- Builds the respective letnum         
buildNum :: Parser LetExpr
buildNum = do 
         space
         n <- nat
         return (LetNum n)
         
-- Builds the respective lamVar            
buildLetVar :: Parser LetExpr
buildLetVar = do
              expr <- var
              return (LetVar expr)

-- Builds the list of lamvariables which are broken by semicolons              
buildVarList :: Parser [Int]
buildVarList = do
          list <- many (do {space; var})
          return list
                   
              
-- builds the respecitve letfunction             
buildLetFunc = do
              expr <- func
              return (LetFun expr)
 
-- data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)

--buildLetDef :: Parser LetDef
-- Breaks the equation into its ;
breakEquation = do
                expr <- buildList `sepby1` (char ';')
                return expr
                
-- Main LetDef function which looks for the correct syntax of the f x1 x2... = cont... 
buildList = do
             f <- func
             vars <- buildVarList
             space
             char '='
             v <- rest
             return (f:vars,v)
             
-- Main final letDef which combines all the functions before the generate the "let blah blah..." 
buildLetDef = do
              symbol "let"
              bruh <- breakEquation
              symbol "in"
              v2 <- rest
              return (LetDef bruh v2)
              
 
 -- Grabs the first valid syntax from the functions above
checkSyntax = buildNum <|> buildLetVar <|> brackets <|> buildLetDef <|> buildLetFunc

-- Takes into account brackets which is used below 
brackets =
         do
         space
         char '('
         e <- rest
         char ')'
         return e
         
         
-- Functions derived from Wojciech Różowski which does assosiation 
-- Thanks for the help 

rest = buildLetApp <|> checkSyntax
        
buildLetApp :: Parser LetExpr
buildLetApp =
    do
        ex <- some checkSyntax
        return (foldl1 (\e1 -> \e2 -> (LetApp e1 e2)) ex)
-- Done
              
 
 

-- Challenge 5
-- translate a let expression into lambda calculus, using Scott numerals
-- convert let symbols to lambda variables using Jansen's techniques rather than Y
-- Discussed with Josh Hook and Benjamin Thompson

--letToLambda :: LetExpr -> LamExpr

-- Starts off by setting up the conditions
-- Removes clashes of the function names and variable names and begins doing the substitution after the 'in'
letToLambda (LetDef a b) = (topLevel (LetDef ((avoidClash a) (getFunctions a [] [])) b))
letToLambda (LetApp a b) = LamApp (letToLambda a) (letToLambda b)
letToLambda (LetFun a) = (LamVar a)
letToLambda (LetVar a) = (LamVar a)
letToLambda (LetNum a) = (convertNumToScott a 0)

convertNumToScott 0 starting = (LamAbs starting (LamAbs (starting+1) (LamVar starting)))
convertNumToScott number starting = (LamAbs starting (LamAbs (starting + 1) (LamApp (LamVar (starting+ 1)) (convertNumToScott (number-1) (starting+1)))))


-- Functions to rename clash variables, including helpers and iterators
-------------------------------------------------------------------------------------------------------------------------------
getClashFunctions (LetVar a ) = []
getClashFunctions (LetNum a ) = []
getClashFunctions (LetFun a ) = a : []
getClashFunctions (LetApp a b) = (getClashFunctions a) ++ (getClashFunctions b)


-- Changes the number in a let expression 
changeClash (LetApp a b) look change = LetApp (changeClash a look change) (changeClash b look change)
changeClash (LetFun a) look change = LetFun a
changeClash (LetNum a) look change = LetNum a

changeClash (LetVar a) look change | a == look = (LetVar change)
                              | otherwise = LetVar a

getClashVariables (LetNum a) = []                              
getClashVariables (LetVar a) = []
getClashVariables (LetFun a) = a : []
getClashVariables (LetApp a b) = (getClashVariables a) ++ (getClashVariables b)                             


-- Pattern matches going through the entire let expression                              
avoidClash [] _ = []                              
avoidClash ((a,LetApp c b):xs) functions = ((iterateThroughApp (totalFunctions) (tail a) (LetApp c b) (totalFunctions) (firstFunction)) : (avoidClash xs functions))
                               where 
                               totalFunctions = ((getClashFunctions (LetApp c b)) ++  (firstFunction: functions))
                               firstFunction = (head a)
                               
                               
avoidClash ((a,LetVar b):xs) functions = ((iterateThroughVar (firstFunction: functions) (tail a) b (firstFunction: functions) (firstFunction))) : (avoidClash xs functions) 
                               where 
                               firstFunction = (head a)
                     
avoidClash ((a,LetFun b):xs) functions = ((iterateThroughFun (b : firstFunction: functions) (tail a) b (b : firstFunction: functions) (firstFunction))) : (avoidClash xs functions) 
                               where 
                               firstFunction = (head a)
                               
avoidClash ((a,LetNum b):xs) functions = ((iterateThroughNum (firstFunction: functions) (tail a) b (firstFunction: functions) (firstFunction))) : (avoidClash xs functions) 
                               where 
                               firstFunction = (head a)                               
                               
avoidClashReplace (num) (replace) xs = map (\h -> if h == num then replace else h) xs


-- Respective iterators for Applications, Variables, Numbers and Functions
iterateThroughApp [] list check unchanged firstFunction = (firstFunction : list, check)
iterateThroughApp (f: functions) list check unchanged firstFunction | f `elem` list && f `elem` (getClashVariables check) = iterateThroughApp functions (avoidClashReplace f (getMax unchanged) list) (changeClash check f (getMax unchanged)) (getMax unchanged : unchanged) firstFunction
                                                                    | f `elem` list = iterateThroughApp functions (avoidClashReplace f (getMax unchanged) list) check (getMax unchanged : unchanged) firstFunction
                                                                    | otherwise = iterateThroughApp functions list check unchanged firstFunction

                                                                    
iterateThroughVar [] list check unchanged firstFunction = (firstFunction : list, LetVar check)
iterateThroughVar (f:functions) list check unchanged firstFunction | f `elem` list && f == check = iterateThroughVar functions (avoidClashReplace f (getMax unchanged) list) (getMax unchanged) (getMax unchanged : unchanged) firstFunction
                                        | f `elem` list = iterateThroughVar functions (avoidClashReplace f (getMax unchanged) list) check (getMax unchanged : unchanged) firstFunction
                                        | otherwise = iterateThroughVar functions list check unchanged firstFunction
                                        
iterateThroughNum [] list check unchanged firstFunction = (firstFunction : list, LetNum check)
iterateThroughNum (f:functions) list check unchanged firstFunction | f `elem` list = iterateThroughNum functions (avoidClashReplace f (getMax unchanged) list) check (getMax unchanged : unchanged) firstFunction
                                                                   | otherwise = iterateThroughNum functions list check unchanged firstFunction                                        
                                        
iterateThroughFun [] list check unchanged firstFunction = (firstFunction : list, LetFun check)
iterateThroughFun (f:functions) list check unchanged firstFunction | f `elem` list = iterateThroughFun functions (avoidClashReplace f (getMax unchanged) list) check (getMax unchanged : unchanged) firstFunction
                                                                   | otherwise = iterateThroughFun functions list check unchanged firstFunction                                        

                                        
------------------------------------------------------------------------------------------                              

                              
-- Does parallel subsitution for after the 'in' in the let def                               
topLevel (LetDef a b) = generateTopLevelReduction (firstRename b (getExpressionNames b) 0) (zip firstList (fmap ((\x -> [x])) (getCorrectPrime b (buildPrimes (LetDef a b) ) ) ) )
                      where 
                      (firstList) = [x | (x,y) <- (getMapping b (getExpressionNames b) (checkFunctions b [] []) 0)]
                      

-- Makes a list of functions that follow the pattern of f'f'g' and g'f'g' and assossiate it with its respective function number                      
buildPrimes (LetDef a b) =  zip ((getFunctions a [] [])) (((buildNewList (stepOne a a) ((stepOne a a)))))

-- Returns the build prime list but in the correct order which is dependent on after the 'in'
getCorrectPrime (LetApp a b) xs = (getCorrectPrime a xs) ++ (getCorrectPrime b xs) 
getCorrectPrime (LetFun a) xs = [y | (x,y) <- xs, a == x] 
getCorrectPrime (LetVar a) _ = [(LamVar a)]
getCorrectPrime (LetNum a) xs = [convertNumToScott a ((getMaxTuple xs) + 1) ]

--Helper function to get max of a tuple list

getMaxTuple xs = maximum [x | (x,y) <- xs] 
-- This does the parallel substitution at the top level, looks for the mapping built and returns the value from the key 
generateTopLevelReduction (LamApp a b) xs = LamApp (generateTopLevelReduction a xs) (generateTopLevelReduction b xs)
generateTopLevelReduction (LamAbs a b) xs = LamAbs a (generateTopLevelReduction b xs)
generateTopLevelReduction (LamVar a) xs | contained xs a == [] = LamVar a
                            | otherwise = head (contained xs a)

-- This creats the application of the f' followed by g' etc...
-- e.g f'f'g'                            
buildNewList [] _ = []
buildNewList (x:xs) rest = [(foldl1 (\e1 -> \e2 -> (LamApp (e1) (e2))) (x:rest))] ++ (buildNewList xs rest)


-- Step one is the first step which returns a list of f' and the g's  etc from the let definition
stepOne [] _ = []
stepOne ((xs,expr):xss) unchanged = [buildAbstraction ((getFunctions unchanged [] [])++(tail xs)) expr (getFunctions unchanged [] []) (getExpressionNames expr) ] ++ (stepOne xss unchanged)

---------------------------------------------------------------------                        
getMaxReduction expr | expr == eval1cbn expr = expr
                     | otherwise = getMaxReduction (eval1cbn expr)

eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2)  = (LamApp (eval1cbn e1) e2)
---------------------------------------------------------------------

-- Build abstraction does the first step of creating the primes functions by generate the abstraction of the list of the original function e.g f0 -> f1 -> f2 etc...
-- Once it reaches the end it does a parallel substitution by generating a mapping and then replacing it with the generatePrime function
buildAbstraction [] expr functions listOfNames = generatePrime (firstRename expr listOfNames 0) (getMapping expr listOfNames functions 0)
buildAbstraction (x:xs) expr functions listOfNames = LamAbs x (buildAbstraction xs expr functions listOfNames)


-- Gets the mapping of the first rename
getMapping (LetApp a b) listOfNames functions counter = (getMapping a listOfNames functions counter) ++ (getMapping b listOfNames functions (countFunctions a)) 
getMapping (LetFun a) listOfNames functions counter = [(getNthFree counter listOfNames, (a : functions))]
getMapping (LetVar a) listOfNames functions counter = []
getMapping (LetNum a) listOfNames functions counter = []


-- Does the first rename for the parallel substitution 
firstRename (LetApp a b) listOfNames counter = LamApp (firstRename a listOfNames counter) (firstRename b listOfNames (countFunctions a))
firstRename (LetFun a) listOfNames counter = subst (LamVar a) a (LamVar (getNthFree counter listOfNames))
firstRename (LetVar a) listOfNames counter = (LamVar a)
firstRename (LetNum a) listOfNames counter = (convertNumToScott a (getNthFree counter listOfNames))


-- Helper function which counts the functions in order to make renaming possible
countFunctions (LetApp a b) = countFunctions a + countFunctions b
countFunctions (LetFun a) = 1
countFunctions (LetVar a) = 0
countFunctions (LetNum a) = 0

-- Does the replacement based on the mapping and then builds the application 
generatePrime (LamApp a b) xs = LamApp (generatePrime a xs) (generatePrime b xs)
generatePrime (LamAbs a b) xs = LamAbs a (generatePrime b xs)
generatePrime (LamVar a) xs | contained xs a == [] = LamVar a
                            | otherwise = buildApp (contained xs a)


-- Helper functions for renaming                            
getExpressionNames (LetApp a b) = (getExpressionNames a) ++ (getExpressionNames b)
getExpressionNames (LetVar a) = [a]
getExpressionNames (LetFun a) = [a]
getExpressionNames (LetNum a) = []

-- Helper function for finding mapping                            
contained xs element | length list > 0 = head list
                     | otherwise = []
                     where
                       list = [b | (a,b) <- xs, element == a]

-- Functions which return the functions list (UNIQUE)                      
----------------------------------------------------------------
getFunctions expr [] = helper (getFunctionsTrue expr [])

-- Removes duplicates
helper [] _ = []
helper (x:xs) seen | x `elem` seen = helper xs seen
                   | otherwise = x : helper xs (x:seen)
                   
checkFunctions expr seen = helper (checkFunctionsTrue expr seen)  
                   
                   
  
getFunctionsTrue [] _ = []
getFunctionsTrue ((xs,expr):xss) seen | (head xs) `elem` seen = (checkFunctions expr seen []) ++ (getFunctionsTrue xss seen) 
                                  | otherwise = [(head xs)] ++ (checkFunctions expr ((head xs):seen) []) ++ (getFunctionsTrue xss ((head xs):seen)) 
----------------------------------------------------------------

checkFunctionsTrue (LetApp a b) seen = (checkFunctionsTrue a seen) ++ (checkFunctionsTrue b seen)
checkFunctionsTrue (LetFun a) seen | a `elem` seen = []
                               | otherwise = [a]
checkFunctionsTrue (LetDef a b) seen = (getFunctionsTrue a seen) ++ (checkFunctionsTrue b seen)
checkFunctionsTrue _ _ = []
 ----------------------------------------------------------------
 

getNthFree 0 xs = getMax xs
getNthFree n xs = getNthFree (n-1) (getMax xs : xs)  


buildApp xs = (foldl1 (\e1 -> \e2 -> (LamApp (e1) (e2))) (fmap (LamVar) xs))


-- data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)


-- Challenge 6
-- convert a lambda calculus expression into one using let expressions and application
-- can use lambda lifting techniques described in wikipedia article
-- Discussed with Ben Thompson and Josh Hook
-- data LetExpr = LetApp LetExpr LetExpr  |  LetDef [([Int], LetExpr)] LetExpr |  LetFun Int | LetVar Int | LetNum Int deriving (Show, Eq)

lambdaToLet :: LamExpr -> LetExpr

lambdaToLet e | (countAllAbstractions e) == 0 = letDefPrefix e [] 0
              | otherwise = LetDef (sortTupleByFun xs) (letDefPrefix e (getTopLvls xs xs) 0)
                      where xs = convert e 0
letDefPrefix (LamApp lE rE) xs c = LetApp (leftSide) (letDefPrefix rE xs ((countLetFuns leftSide) + c))
                                                   where leftSide = letDefPrefix lE xs c
letDefPrefix (LamAbs n e) xs c = buildLetFun (head $ fst $ xs!!c) (foldl (\x y -> y:x) [] (removeDuplicates (getFreeList e') []))
                                                  where e' = (LamAbs n e)
letDefPrefix (LamVar n) xs c = LetVar n


countAllAbstractions (LamApp a b) = (countAllAbstractions a) + (countAllAbstractions b)
countAllAbstractions (LamAbs a b) = 1
countAllAbstractions (LamVar a) = 0


-- Starts by building the let functions for the let expression
buildLetFun letFunName freeList | (length freeList) == 0 = LetFun letFunName
                                        | otherwise = buildMissingVarLetFun freeList letFunName

buildMissingVarLetFun freeList letFunName | (length freeList) == 1 = LetApp (LetFun letFunName) (LetVar (head freeList))
                                                  | otherwise = LetApp (buildMissingVarLetFun (tail freeList) letFunName) (LetVar $ head freeList)

-- deals with the construction of inner most expressions
convert (LamApp lE rE) c = leftSide ++ (convert rE ((length leftSide) + c))
                                                       where leftSide = (convert lE c)
                                                       
convert (LamAbs n e) c | (innerCheck e') == False = outterMost (e') (dropOuter e') (buildAbs e') c
                                         | otherwise                          = [getTuple e' (buildAbs e') c]
                                        where e' = (LamAbs n e)
convert (LamVar n) c = []

-- gets the tuple of an inner most
getTuple e aL fC = ([fC] ++ (removeDuplicates (getFreeList e) []) ++ aL, convertExpression (dropOuter e))

addMissingVar list (LamVar name) | (name `elem` list) == False = [name]
                                 | otherwise                   = []
addMissingVar list (LamApp rE lE) = addMissingVar list lE ++ addMissingVar list rE

-- Does conversion to let from the innermost
convertExpression (LamApp lE rE) = LetApp (convertExpression lE) (convertExpression rE)
convertExpression (LamVar n) = LetVar n

-- After this it deals with the outer most expression
outterMost e (LamApp lE rE) aL c = [(getOutterTuple e e' topLevelOfReturnList aL ((length returnList) + c))] ++ returnList
                                                                             where e' = (LamApp lE rE)
                                                                                   leftSide = (convert lE c)
                                                                                   rightSide = (convert rE ((length leftSide) + c))
                                                                                   returnList = leftSide ++ rightSide
                                                                                   topLevelOfReturnList = (getTopLvls leftSide leftSide) ++ (getTopLvls rightSide rightSide)

-- grabs the tuple for the outer most
getOutterTuple e e' tL aL fC = ([fC] ++ (removeDuplicates (getFreeList e) []) ++ aL, letDefPrefix e' tL 0)

-- Helper functions which sorts the tuples
sortTupleByFun list = sortBy (compare `on` fst) [(x,y) | (x,y) <- list]

-- Grabs the top level expression
getTopLvls [] list = []
getTopLvls (x:xs) list = (containsLetFun x (list \\ [x])) ++ getTopLvls xs list

-- checks if there are any name clashes
containsLetFun tuple [] = [tuple]
containsLetFun tuple (x:xs) | letFunCheck functionNumber (snd x) = []
                            | otherwise                                  = containsLetFun tuple xs                        
                           where functionNumber = head $ fst tuple

letFunCheck fN (LetApp lE rE) = letFunCheck fN lE || letFunCheck fN rE
letFunCheck fN (LetFun n) | n == fN = True
                                         | otherwise              = False
letFunCheck fN (LetVar n) = False

-- Helper function to remove duplicates
removeDuplicates [] list = list
removeDuplicates (x:xs) list | (x `elem` list) = removeDuplicates xs list
                             | otherwise       = removeDuplicates xs (list ++ [x])

-- Helper function which counts the LetFunction
countLetFuns (LetVar n) = 0
countLetFuns (LetFun n) = 1
countLetFuns (LetApp lE rE) = (countLetFuns lE) + (countLetFuns rE)

-- Builds the abstractions given expression
buildAbs (LamApp lE rE) = []
buildAbs (LamAbs n e) = n : buildAbs e
buildAbs (LamVar n) = []

-- gets subexpression 
dropOuter (LamApp lE rE) = (LamApp lE rE)
dropOuter (LamAbs n e) = dropOuter e
dropOuter (LamVar n) = LamVar n

-- check whether an expression is innermost
innerCheck (LamApp (LamAbs n e) rE) = False
innerCheck (LamApp lE (LamAbs n e)) = False
innerCheck (LamApp lE rE) = innerCheck lE && innerCheck rE
innerCheck (LamAbs n e) = innerCheck e
innerCheck (LamVar n) = True



doubles (x:xs) | x > 0 = (x*2) : doubles xs
                 | otherwise = error "REEEE"