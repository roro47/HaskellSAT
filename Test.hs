import SATSolver
import Test.HUnit

-- nnf test

p = Var "p"
q = Var "q"
r = Var "r"

-- formula cnf formula
cnf1 = Const True
cnf2 = Const False
cnf3 = p
cnf4 = Not p
cnf5 = Or [Not p, q]
cnf6 = And [p, Or [Not p, Not q]]
cnf7 = And [p, Not p]
cnf8 = And [Or [Not p, Not q, r], Or [q, p], Or [Not r, p]]

nnfInput1 = p
nnfOutput1 = nnfInput1

nnfInput2 = Not (And [p, q])
nnfOutput2 = Or [Not p, Not q]

nnfInput3 = Not (Or [p, q])
nnfOutput3 = And [Not p, Not q]

nnfInput4 = Not (Imply p (And [p, q]))
nnfOutput4 = And [p, Or [Not p, Not q]]

cnfInput1 = Iff p (Imply q r)
cnfOutput = And [Or [Not p, Not q, r], Or [q, p], Or [Not r, p]]

satInput1 = cnf1
satOutput1 = True

satInput2 = cnf2
satOutput2 = False

satInput3 = cnf3
satOutput3 = True

satInput4 = cnf4
satOutput4 = True

satInput5 = cnf5
satOutput5 = True

satInput6 = cnf6
satOutput6 = True

satInput7 = cnf7
satOutput7 = False

satInput8 = cnf8
satOutput8 = True

genTest (a, b)
  = TestCase (assertEqual ("for "++ show a) b (nnf a))

label xs prefix = zip xs testname
  where testname = [prefix ++ show n | n <- [1..length xs]]
  
nnftests = [(nnfInput1, nnfOutput1),
             (nnfInput2, nnfOutput2),
             (nnfInput3, nnfOutput3),
             (nnfInput4, nnfOutput4)]

cnftests = [(cnfInput1, cnfOutput1),
             (cnfInput2, cnfOutput2),
             (cnfInput3, cnfOutput3),
             (cnfInput4, cnfOutput4),
             (cnfInput5, cnfOutput5),
             (cnfInput6, cnfOutput6),
             (cnfInput7, cnfOutput7),
             (cnfInput8, cnfOutput8)]

nnfTest = map (\(t, l) -> TestLabel l t ) (label nnftests "nnfTest")
cnfTest = map (\(t, l) -> TestLabel l t ) (label nnftests "cnfTest")

tests = TestList [TestLabel (nnfTest ++ cnfTest)]
