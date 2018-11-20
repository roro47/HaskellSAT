module Convert where

import Formula

-- Utility

extract :: Formula -> [Formula]
extract (And fs) = fs
extract (Or fs) = fs
extract f = [f]
  
isAnd :: Formula -> Bool
isAnd (And _) = True
isAnd _ = False

isOr :: Formula -> Bool
isOr (Or _) = True
isOr _ = False

-- utility function for distribute
sortAndFirst :: [Formula] -> [Formula]
sortAndFirst fs
  = filter isAnd fs ++ filter (not . isAnd) fs


-- Functions for converting formula



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


-- Remove Or, And layer
-- For example, Or [Or [T, F], Or [T, T]] 
-- should be simplified to Or [T, F, T, T]
flattenF :: Formula -> Formula
flattenF f
  = case f of
      Or fs  -> Or (concat $
                    map (\f -> if isOr f
                               then handleFs (extract f)
                               else [flattenF f])
                         fs
                   )
      And fs -> And (concat $
                    map (\f -> if isAnd f
                               then handleFs (extract f)
                               else [flattenF f])
                          fs
                    )
      otherwise -> f

   where handleFs :: [Formula] -> [Formula]
         handleFs fs = map (\f -> flattenF f) fs

-- Given that all the And conjuctions are in the front
distribute :: Formula -> Formula
distribute (And fs) = And (map distribute fs)
distribute all@(Or fss)
  = case f of
      And fs' ->
        And (map (\f ->
                    distribute (Or (sortAndFirst ([f] ++ fs)))) fs')
      otherwise -> all
   where (f:fs) = sortAndFirst fss
distribute f = f 

-- convert a formula into cnf form
cnf :: Formula -> Formula
cnf f = flattenF $ distribute nnf'
  where nnf' = flattenF $ nnf f

