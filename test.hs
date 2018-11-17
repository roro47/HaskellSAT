import SATSolver
import Test.HUnit

-- nnf test

v1 = Var "x"
v2 = Var "y"

nnfInput1 = v1
nnfOutput1 = nnfInput1

nnfInput2 = Not (And [v1, v2])
nnfOutput2 = Or [Not v1, Not v2]

nnfInput3 = Not (Or [v1, v2])
nnfOutput3 = And [Not v1, Not v2]

nnfInput4 = Not (Imply v1 (And [v1, v2]))
nnfOutput4 = And [v1, Or [Not v1, Not v2]]

nnfTest1 
  = TestCase (assertEqual ("for "++ show nnfInput1) nnfOutput1 (nnf nnfInput1))

nnfTest2 
  = TestCase (assertEqual ("for " ++ show nnfInput2) nnfOutput2 (nnf nnfInput2)) 

nnfTest3
  = TestCase (assertEqual ("for " ++ show nnfInput3) nnfOutput3 (nnf nnfInput3))

nnfTest4
  = TestCase (assertEqual ("for " ++ show nnfInput4) nnfOutput4 (nnf nnfInput4))


tests = TestList [TestLabel "nnfTest1" nnfTest1,
                  TestLabel "nnfTest2" nnfTest2,
                  TestLabel "nnfTest3" nnfTest3,
                  TestLabel "nnfTest4" nnfTest4]
