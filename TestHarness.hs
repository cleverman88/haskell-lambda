-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2019
-- Sample tests by Andy Gravell, Julian Rathke
-- Additional tests by Sohaib Karous
-- DO NOT RE-DISTRIBUTE OR RE-POST

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- and solutions to challenges
import Data.Char
--import Parsing
import Challenges


-- Main program
--
-- There is one simple test suite per exercise, each a list of assertions
-- These are only the published tests -- further, unseen tests will also be applied
--
simpleTests :: [[Bool]]
simpleTests =
    [ -- Challenge 1
      [ alphaNorm (LamApp (LamVar 1) (LamVar 0)) == LamApp (LamVar 1) (LamVar 0),
        alphaNorm (LamAbs 3 (LamVar 2)) == LamAbs 0 (LamVar 2),
        alphaNorm (LamAbs 0 (LamAbs 1 (LamVar 0))) == LamAbs 0 (LamAbs 1 (LamVar 0)),
        alphaNorm (LamAbs 1 (LamAbs 0 (LamVar 1))) == LamAbs 0 (LamAbs 1 (LamVar 0)),
        alphaNorm (LamAbs 1 (LamAbs 0 (LamVar 0))) == LamAbs 0 (LamAbs 0 (LamVar 0)),
        alphaNorm (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0)))) == 
            LamAbs 0 (LamAbs 1 (LamAbs 1 (LamVar 0))),
        alphaNorm (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) == (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3)) (LamApp (LamAbs 0 (LamVar 0)) (LamVar 5))),
        ---
        alphaNorm (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) == (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 0 (LamVar 0))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 0 (LamAbs 0 (LamVar 1))) (LamAbs 0 (LamVar 1))))),
        ---
        alphaNorm (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) == (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))),
        ---
        alphaNorm (LamApp (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)) (LamAbs 0 (LamVar 1))) == (LamApp (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)) (LamAbs 0 (LamVar 1))),
        ---
        alphaNorm (LamApp (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))) (LamAbs 0 (LamAbs 1 (LamAbs 4 (LamAbs 5 (LamVar 3)))))) (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 4 (LamAbs 5 (LamVar 3))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))) (LamAbs 0 (LamAbs 1 (LamAbs 4 (LamAbs 5 (LamVar 3))))))) == 
        (LamApp (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))) (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamVar 3)))))) (LamApp (LamApp (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamVar 3))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 2 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))) (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamAbs 0 (LamVar 3)))))))
      ],
      [ -- Challenge 2
        countAllReds (LamAbs 0 (LamAbs 1 (LamVar 1))) 0 == 0,
        countAllReds (LamAbs 0 (LamAbs 1 (LamVar 1))) 1 == 1,
        countAllReds (LamAbs 0 (LamAbs 1 (LamVar 1))) 2 == 1,
        countAllReds (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 1 (LamVar 1))) 0 == 0,
        countAllReds (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 1 (LamVar 1))) 1 == 0,
        countAllReds (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 1 (LamVar 1))) 2 == 1,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))
           (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 2 == 0,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))
           (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 3 == 1,
           countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 0 == 0,
           countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 1 == 0,
           countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 2 == 0,
           countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 3 == 1,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) 4 == 3,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2))))) 1 == 0,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2))))) 2 == 0,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2))))) 3 == 0,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2))))) 4 == 0,
        countAllReds (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2))))) 5 == 1,
        countAllReds (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) 1 == 0,
        countAllReds (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) 2 == 0,
        countAllReds (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) 3 == 0,
        countAllReds (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) 4 == 0,
        countAllReds (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) 5 == 12
      ],
      [ -- Challenge 3 (but note that additional spaces will be accepted)
        printLambda (LamApp (LamVar 2) (LamVar 1)) == "x2 x1",
        printLambda (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) == 
            "(\\x1 -> x1) \\x1 -> x1",
        printLambda (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) == 
            "\\x1 -> x1 \\x1 -> x1", 
        printLambda (LamAbs 1 (LamAbs 2 (LamVar 1))) == "0",
        printLambda (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))) == "1",
        printLambda ((LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)))) == "0 x3 ((\\x4 -> x4) x5)",
        printLambda (LamApp (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) (LamApp (LamApp (LamAbs 0 (LamVar 0)) (LamVar 4)) (LamApp (LamAbs 5 (LamAbs 4 (LamVar 1))) (LamAbs 0 (LamVar 1))))) == "(\\x1 -> x1) \\x2 -> x2 ((\\x0 -> x0) x4 ((\\x5 -> \\x4 -> x1) \\x0 -> x1))",
        printLambda (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))))) == "2",
        printLambda (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) ((LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))))))))) == "3",
        printLambda (LamAbs 2 (LamAbs 2 (LamApp (LamVar 2) ((LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) ((LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))))))))))))) == "4"
        
      ],
      [ -- Challenge 4
        parseLet "let x1 = x2" == Nothing,
        parseLet "x1 (x2 x3)" == Just (LetApp (LetVar 1) (LetApp (LetVar 2) (LetVar 3))),
        parseLet "x1 x2 x3" == Just (LetApp (LetApp (LetVar 1) (LetVar 2)) (LetVar 3)),
        parseLet "let f1 x1 = x2 in f1 x1" == 
            Just (LetDef [([1,1],LetVar 2)] (LetApp (LetFun 1) (LetVar 1))),
        parseLet "let f1 x2 = x2; f2 x1 = x1 in f1 x1" ==
            Just (LetDef [([1,2],LetVar 2),([2,1],LetVar 1)] (LetApp (LetFun 1) (LetVar 1))),
        parseLet "let f1 f1 x2 = x3; f3 = f2 in f1" == Nothing,
        parseLet "let f2 x3 x4 = x5; f3 x6 x7 = x3 in f2 f3" == Just (LetDef [([2,3,4],LetVar 5),([3,6,7],LetVar 3)] (LetApp (LetFun 2) (LetFun 3))),
        parseLet "let f1 = f1 in f1" == Just (LetDef [([1],LetFun 1)] (LetFun 1)),
        parseLet "let x1 = x2 in x3" == Nothing,
        parseLet "let x1 = x2; x1 = x3; x1 = x4; in f1" == Nothing,
        parseLet "let f0 = f1; f1 = f2; f2 = f3; f3 = f4; f5 = f6; f6 = x1 in f0" == Just (LetDef [([0],LetFun 1),([1],LetFun 2),([2],LetFun 3),([3],LetFun 4),([5],LetFun 6),([6],LetVar 1)] (LetFun 0))
      ],
      
       
      [ -- Challenge 5 (but note that equivalent answers will be accepted)
        (letToLambda (LetDef [([0],LetFun 0)] (LetFun 0))) ==
          (getMaxReduction (LamApp (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))) (LamAbs 0 (LamApp (LamVar 0) (LamVar 0))))),
        alphaNorm (getMaxReduction (letToLambda (LetDef [([1,2],LetVar 2)] (LetFun 1)))) ==
          (alphaNorm $ getMaxReduction (LamApp (LamAbs 0 (LamAbs 0 (LamVar 0))) (LamAbs 0 (LamAbs 0 (LamVar 0))))),
        (alphaNorm $ getMaxReduction (letToLambda (LetDef [([1,2,3],LetApp (LetVar 3) (LetVar 2))] (LetFun 1)))) ==
          (alphaNorm $ getMaxReduction (LamApp (LamAbs 0 (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))) (LamAbs 0 (LamAbs 0 (LamAbs 1 (LamApp (LamVar 1) (LamVar 0))))))),
        (getMaxReduction (letToLambda (LetDef [([0,0],LetFun 1),([1,1],LetVar 1)] (LetFun 0)))) ==
          (getMaxReduction (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 2)))))),
        (letToLambda (LetDef [([0,0,1],LetVar 0),([1,1],LetApp (LetApp (LetFun 0) (LetVar 1)) (LetFun 1))] (LetFun 1))) ==
          ((LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamApp (LamVar 0) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 2)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamApp (LamVar 0) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))))))),
       (letToLambda (LetDef [([1,2],LetVar 2),([2,1],LetVar 1)] (LetApp (LetFun 1) (LetVar 1)))) == (LamApp (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamAbs 0 (LamVar 0)))) (LamAbs 1 (LamAbs 2 (LamAbs 0 (LamVar 0))))) (LamAbs 1 (LamAbs 2 (LamAbs 0 (LamVar 0))))) (LamVar 1)),
       (letToLambda (LetDef [([2,3,4],LetVar 5),([3,6,7],LetVar 3)] (LetApp (LetFun 2) (LetFun 3)))) == (LamApp (LamApp (LamApp (LamAbs 2 (LamAbs 3 (LamAbs 0 (LamAbs 4 (LamVar 5))))) (LamAbs 2 (LamAbs 3 (LamAbs 0 (LamAbs 4 (LamVar 5)))))) (LamAbs 2 (LamAbs 3 (LamAbs 6 (LamAbs 7 (LamVar 3)))))) (LamApp (LamApp (LamAbs 2 (LamAbs 3 (LamAbs 6 (LamAbs 7 (LamVar 3))))) (LamAbs 2 (LamAbs 3 (LamAbs 0 (LamAbs 4 (LamVar 5)))))) (LamAbs 2 (LamAbs 3 (LamAbs 6 (LamAbs 7 (LamVar 3))))))),
       (letToLambda (LetDef [([0],LetFun 1),([1],LetFun 2),([2],LetFun 3),([3],LetFun 4),([5],LetFun 6),([6],LetVar 1)] (LetFun 0))) == (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6)))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 2) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6)))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 3) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6)))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 4) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6)))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamApp (LamVar 6) (LamVar 0)) (LamVar 1)) (LamVar 2)) (LamVar 3)) (LamVar 4)) (LamVar 5)) (LamVar 6)))))))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamAbs 6 (LamVar 1))))))))), 
       (letToLambda (LetDef [([0],LetFun 1),([1,1],LetVar 2),([2,3],LetVar 4)] (LetApp (LetFun 1) (LetFun 2)))) == (LamApp (LamApp (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 2))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)) (LamVar 2)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 2)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 4)))))) (LamApp (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 4))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)) (LamVar 2)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 2)))))) (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamVar 4))))))),
       (letToLambda (LetDef [([0],LetFun 1)] (LetFun 0))) == LamApp (LamAbs 0 (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1)))) (LamAbs 0 (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 1))))
      ],
      
      [ -- Challenge 6 (but note that equivalent answers will be accepted)
        lambdaToLet (LamAbs 0 (LamVar 0)) ==
          LetDef [([0,0],LetVar 0)] (LetFun 0),
        lambdaToLet (LamApp (LamVar 1) (LamAbs 0 (LamVar 0))) ==
          LetDef [([0,0],LetVar 0)] (LetApp (LetVar 1) (LetFun 0)),
        lambdaToLet (LamApp (LamAbs 0 (LamVar 0)) (LamVar 1)) ==
          LetDef [([0,0],LetVar 0)] (LetApp (LetFun 0) (LetVar 1)),
        lambdaToLet (LamApp (LamAbs 0 (LamVar 0)) (LamAbs 0 (LamVar 0))) ==
          LetDef [([0,0],LetVar 0),([1,0],LetVar 0)] (LetApp (LetFun 0) (LetFun 1)),
        lambdaToLet (LamAbs 0 (LamApp (LamVar 0) (LamAbs 1 (LamApp (LamVar 0) (LamVar 1))))) ==
          LetDef [([0,0,1],LetApp (LetVar 0) (LetVar 1)),([1,0],LetApp (LetVar 0) (LetApp (LetFun 0) (LetVar 0)))] (LetFun 1),
      lambdaToLet (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) ((LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))))))))) == LetDef [([0,1,2],LetVar 1),([1,1,1],LetApp (LetVar 1) (LetFun 0)),([2,1,1],LetApp (LetVar 1) (LetFun 1)),([3,1,1],LetApp (LetVar 1) (LetFun 2))] (LetFun 3),
      lambdaToLet (LamApp (LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamVar 3))(LamApp (LamAbs 4 (LamVar 4)) (LamVar 5))) == LetDef [([0,0,1],LetVar 0),([1,4],LetVar 4)] (LetApp (LetApp (LetFun 0) (LetVar 3)) (LetApp (LetFun 1) (LetVar 5))),
      lambdaToLet ((LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1)))))))))) == LetDef [([0,1,2],LetVar 1),([1,1,1],LetApp (LetVar 1) (LetFun 0)),([2,1,1],LetApp (LetVar 1) (LetFun 1))] (LetFun 2),
      lambdaToLet (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamVar 0)))) == LetDef [([0,0,1,2],LetVar 0)] (LetFun 0),
      lambdaToLet (LamApp (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3)) (LamAbs 0 (LamVar 1))) == LetDef [([0,1,0],LetVar 1)] (LetApp (LetApp (LetApp (LetVar 1) (LetVar 2)) (LetVar 3)) (LetApp (LetFun 0) (LetVar 1)))
      
      ]
    ]
    


eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2)  = (LamApp (eval1cbn e1) e2)

getMaxReduction expr | expr == eval1cbn expr = expr
                     | otherwise = getMaxReduction (eval1cbn expr)
                     
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
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x==y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x/= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)
getAllTaken (LamVar x) = [x]
getAllTaken (LamAbs x e) = [x] ++ (getAllTaken (e))
getAllTaken (LamApp f g) = (getAllTaken f) ++ (getAllTaken g)
-- The main program checks and displays the results of the tests 
--
main :: IO ()
main = 
  do
    putStrLn "... Testing ..."
    simpleTestSuite simpleTests 
    putStrLn "... Completed ..."

-- process one test suite at a time
simpleTestSuite :: [[Bool]] -> IO ()
simpleTestSuite [] = 
  do
    putStr ""
simpleTestSuite (bs : bbs) =
  do
    putStrLn ("  " ++ show (length [b | b <- bs, b]) ++ " tests passed out of " ++ show (length bs))
    simpleTestSuite bbs

