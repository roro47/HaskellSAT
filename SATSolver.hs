module SATSolver where

import Formula
import Convert

import Data.Maybe
import Data.List

-- pre: f is a literal, fs is a clause
-- replacing literal with True
unitResolve :: Formula -> Formula -> Formula
unitResolve f l = assignVar (l, Const True) f

-- decide whether a formula is a literal or not
isLiteral :: Formula -> Bool
isLiteral f
  = case f of
      Const _ -> True
      Var s -> True
      otherwise -> False

-- Assuming input formula is in cnf form
-- i.e. = And [ Or [..], Or [..], Or [..]] 
-- is boolean constrain propagation unique or not
bcp :: Formula -> Formula
bcp f@(And fs)
  | literals == [] = (And fs)
  | otherwise = bcp f'
  where literals = filter isLiteral fs
        f' = foldl (\form lit -> eval $ unitResolve form lit) f literals
bcp f = f

-- pre: given formula is cnf
-- get the first unassigned variable
getVar :: Formula -> Maybe Formula
getVar (Const _) = Nothing
getVar (Var s) = Just (Var s)
getVar (Not f) = getVar f
getVar (Or fs) = if l' == [] then Nothing else head l'
  where l' = filter (\f -> f /= Nothing) (map getVar fs)
getVar (And fs) = if l' == [] then Nothing else head l'
  where l' = filter (\f -> f /= Nothing) (map getVar fs)

-- pre: given formula is in cnf form
chooseVar :: Formula -> Maybe Formula
chooseVar f = getVar f

-- pre: formula to transform is in cnf
--      f in (f, b) is Var s
--      b in (f, b) is either T or F, 
assignVar :: (Formula, Formula) -> Formula -> Formula
assignVar (f, b) (And fs) = And (map (\f' -> assignVar (f, b) f') fs)
assignVar (f, b) (Or fs) = Or (map (\f'' -> assignVar (f, b) f'') fs)
assignVar (f, b) (Not f') = if f == f' then b' else f'
  where b' = if b == Const False then Const True else Const False 
assignVar (f, b) f' = if f == f' then b else f'

-- pre: formula to transform is in cnf
-- used after assignVar to reduce formula
eval :: Formula -> Formula
eval (And fs) = case length fs' of
                   0 -> Const True
                   1 -> head fs'
                   otherwise -> And fs'
  where fs' = nub $ filter (/= (Const True) ) $ map eval fs
eval (Or fs)
  | elem (Const True) fs = Const True
  | otherwise = case length fs' of
                   0 -> Const False
                   1 -> head fs'
                   otherwise -> Or fs'
    where fs' = nub $ filter (/= (Const False)) fs
eval f = f

{-
plp :: Formula -> Formula
-}

dpll :: Formula -> Bool
dpll (Const b) = b
dpll f = case var of
           Just (Var s)-> if dpll (eval $
                                   assignVar (Var s, Const True) f')
                          then True
                          else dpll (eval $
                                     assignVar (Var s, Const False) f')
           otherwise -> dpll f'
  where f' = bcp f
        var = getVar f'


sat :: Formula -> Bool
sat f = dpll f'
  where f' = eval $ cnf f
