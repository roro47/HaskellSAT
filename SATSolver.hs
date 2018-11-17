
module SATSolver where
import Data.Maybe

data Formula = T 
             | F  
             | Var String
             | Not Formula
             | And [Formula]
             | Or [Formula]
             | Imply Formula Formula
             | Iff Formula Formula deriving (Show, Eq)


-- convert formula so that
-- 1. only connectives are Not, And, Or
-- 2. Negations only applied to literals
nnf :: Formula -> Formula
nnf (Imply f1 f2) = nnf (Or [Not f1, f2])
nnf (Iff f1 f2)  = nnf (And [Imply f1 f2, Imply f2 f1])

nnf (Not (And fs)) = nnf (Or (map (\f -> nnf $ Not f) fs))
nnf (Not (Or fs)) = nnf (And (map (\f -> nnf $ Not f) fs))
nnf (Not (Not f)) = nnf f
nnf (Not f@(Imply _ _)) = nnf (Not (nnf f))
nnf (Not f@(Iff _ _)) = nnf (Not (nnf f))
nnf (Not f) = Not (nnf f)
nnf (And fs) = And (map (\f -> nnf f) fs)
nnf (Or fs) = Or (map (\f -> nnf f) fs)
nnf f = f


-- Assuming given an And formula
extractAnd :: Formula -> [Formula]
extractAnd  (And fs) = fs
--extractAnd f = Nothing

-- Assuming given an Or
extractOr :: Formula -> [Formula]
extractOr (Or fs) = fs
extractOr f = [f]
--extractAnd f = Nothing

isAnd :: Formula -> Bool
isAnd (And _) = True
isAnd _ = False

isOr :: Formula -> Bool
isOr (Or _) = True
isOr _ = False

hasAnd :: [Formula] -> Bool
hasAnd fs = or (map isAnd fs) 

hasOr :: [Formula] -> Bool
hasOr fs = or (map isOr fs)


-- Remove Or, And layer
-- For example, Or [Or [T, F], Or [T, T]] 
-- should be simplified to Or [T, F, T, T]
flattenF :: Formula -> Formula
flattenF f
  = case f of
      Or fs  -> Or (concat $
                    map (\f -> if isOr f
                               then handleFs (extractOr f)
                               else [flattenF f])
                         fs
                   )
      And fs -> And (concat $
                    map (\f -> if isAnd f
                               then handleFs (extractAnd f)
                               else [flattenF f])
                          fs
                    )
      otherwise -> f

   where handleFs :: [Formula] -> [Formula]
         handleFs fs = map (\f -> flattenF f) fs


-- utility function for distribute
sortAndFirst :: [Formula] -> [Formula]
sortAndFirst fs
  = filter isAnd fs ++ filter (not . isAnd) fs


-- Given that all the And conjuctions are in the front
distribute :: Formula -> Formula
distribute all@(Or fss)
  = case f of
      And fs' ->
        And (map (\f ->
                    distribute (Or (sortAndFirst ([f] ++ fs)))) fs')
      otherwise -> all
   where (f:fs) = sortAndFirst fss
distribute f = f 
 
{--
cnf :: Formula -> Formula
cnf f = 
  where f' = removeLayer $ nnf f
        cnf' :: Formula -> Formula
--}

cnf :: Formula -> Formula
cnf f = distribute nnf'
  where nnf' = flattenF $ nnf f

-- Assume inputing literal
isOppo :: Formula -> Formula -> Bool
isOppo (Not f1) f2 = f1 == f2
isOppo f1 (Not f2) = f1 == f2
isOppo _ _ = False


-- check if two formulas are resolvable or not
isResolve :: [Formula] -> [Formula] -> Bool
isResolve fs1 fs2
  = or [isOppo f1 f2 | f1 <- fs1, f2 <- fs2]

-- pre: f is a literal, fs is a clause
-- extra used for checking unit resolve effect or not
unitResolve :: Formula -> Formula -> (Formula, Bool)
unitResolve  f fs
  | elem f (extractOr fs) = (Or [], True)
  | extractOr fs == extractOr fs' = (fs, False)
  | otherwise = (fs', True)
  where fs' =  Or (filter (not . isOppo f) (extractOr fs))

-- Assuming no duplicates list
{-
resolve :: [Formula] -> [Formula] -> [Formula]
resolve xs [] = xs
resolve [] ys = ys
resolve (x:xs) ys
  | length ys' < length ys = resolve xs ys
  | otherwise = x : (resolve xs ys)
  where ys' = unitResolve x ys
-}

isLiteral :: Formula -> Bool
isLiteral f
  = case f of
      Var s -> True
      T -> True
      F -> True
      otherwise -> False

clean :: [Formula] -> [Formula]
clean [] = []
clean ((Or f):fs)
  | f == [] = clean fs
  | length f == 1 = (head f : clean fs)
  | otherwise = ((Or f):(clean fs))
clean (f:fs) = (f:(clean fs))

-- Assuming input formula is in cnf form
-- i.e. = And [ Or [..], Or [..], Or [..]] 
-- is boolean constrain propagation unique or not
bcp :: Formula -> Formula
bcp (Or fs) = bcp' fs
  where bcp' fs
          | maybeliteral == Nothing || not resolved = Or fs
          | otherwise = bcp (Or fs') 
          where maybeliteral = getLiteral fs
                literal = fromJust maybeliteral
                fsBool = map (unitResolve literal) (filter (\f -> f /= literal) fs)
                resolved = or (map snd fsBool)
                fs' = clean $ map fst fsBool
                
getLiteral :: [Formula] -> Maybe Formula
getLiteral fs
  | l == [] = Nothing
  | otherwise = Just (head l)
 where l = filter isLiteral fs

getVar :: Formula -> Just Formula
getVar T = Nothing
getVar F = Nothing
getVar (Var s) = Just (Var s)
getVar (Or fs) = if l' == [] then Nothing else Just (head l')
  where l' = filter (\f -> getVar f \= Nothing) fs
getVar (And fs) = if l' == [] then Nothing else Just (head l')
  where l' = filter (\f -> getVar f \= Nothing) fs

-- pre: given formula is in cnf form
chooseVar :: Formula -> Maybe Formula
chooseVar f = getVar f


-- pre : formula to transform is in cnf
assignVar :: (Formula, Formula) -> Formula -> Formula
assignVar (f, b) (Or fs) = Or (map (\f -> assignVar f) fs)
assignVar (f, b) f' = if f == f' then b else f'


eval :: Formula -> Formula


plp :: Formula -> Formula

dpll :: Formula -> Bool
dpll f
  | f' == T = True
  | f' == F = False
  | otherwise = case var of
                  Var s -> if dpll (eval $ assignVar (var, T) f')
                           then True
                           else dpll (eval $ assignVar (var, F) f')
                  otherwise -> False
  where f' = bcp f
        var = getVar f'
