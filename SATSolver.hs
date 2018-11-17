module SATSolver where

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
unitResolve :: Formula -> Formula -> Formula
unitResolve  f fs
  | elem f fs' = Or []
  | otherwise = filter (not . isOppo f) fs'
  where fs' = extractOr fs

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

-- Assuming input formula is in cnf form
-- i.e. = And [ Or [..], Or [..], Or [..]] 
-- is boolean constrain propagation unique or not
bcp :: Formula -> Maybe Formula
bcp (Or fs) = bcp' fs
  where bcp' fs
          | literal == Nothing = Nothing
          | otherwise = bcp (Or fs') 
          where literal = getLiteral fs
                fs' = map (unitResolve literal) fs
        getLiteral :: [Formula] -> Maybe Formula
        getLiteral fs
          | l == [] = Nothing
          | otherwise = head l
          where l = filter isLiteral fs

 
