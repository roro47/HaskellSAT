module Formula where

import Data.List

data Formula = Const Bool 
             | Var String
             | Not Formula
             | And [Formula]
             | Or [Formula]
             | Imply Formula Formula
             | Iff Formula Formula deriving (Show)


-- for comparing disjunctions and conjunctions
-- ex. Or [ T, F ]  == Or [ F, T ]
--     And [ T, F ] == And [ T, F ]
instance Eq Formula where
  (==) (Const b1) (Const b2) = b1 == b2
  (==) (Var s1) (Var s2) = s1 == s2
  (==) (Not f1) (Not f2) = f1 == f2
  (==) (Imply f1 f2) (Imply f3 f4) = f1 == f3 && f2 == f4
  (==) (Iff f1 f2) (Iff f3 f4) = f1 == f3 && f2 == f4
  (==) (Or fs1) (Or fs2) = null (fs1 \\ fs2) && null (fs2 \\ fs1)
  (==) (And fs1) (And fs2) = null (fs1 \\ fs2) && null (fs2 \\ fs1)
  (==) _ _ = False

  (/=) f1 f2 = not (f1 == f2)

