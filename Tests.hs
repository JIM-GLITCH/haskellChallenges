import Challenges
import Test.Hspec

puzzle1 =
  [ [Wire [North, West], Wire [North, South], Source [North]],
    [Wire [North, West], Wire [East, West], Wire [North, East]],
    [Sink [West], Wire [North, South], Wire [North, West]]
  ]

puzzle2 =
  [ [Wire [South, East], Wire [East, West], Source [West]],
    [Wire [North, East], Wire [East, West], Wire [South, West]],
    [Sink [East], Wire [East, West], Wire [North, West]]
  ]

main :: IO ()
main = hspec $ do 
  describe "challenge1" $ do
    it "puzzle1 should be not complete" $ do
      isPuzzleComplete puzzle1 `shouldBe` False
    it "puzzle2 should be complete" $ do
      isPuzzleComplete puzzle2 `shouldBe` True
  describe "challenge2" $ do
    it "solveCircuit" $ do
      solveCircuit puzzle1 `shouldBe` Just [[R180, R90, R270], [R90, R0, R180], [R180, R90, R0]]
  describe "challenge3" $ do
    it "test1" $ do
      prettyPrint (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))) `shouldBe` "(\\x1 -> x1) \\x1 -> x1"
    it "test2" $ do
      prettyPrint (Let Discard (Var 0) (Abs (V 1) (App (Var 1) (Abs (V 1) (Var 1))))) `shouldBe` "let _ = x0 in \\x1 -> x1 \\x1 -> x1"
    it "test3" $ do
      prettyPrint (Abs (V 1) (Abs Discard (Abs (V 2) (App (Var 2) (Var 1))))) `shouldBe` "\\x1 _ x2 -> x2 x1"
    it "test4" $ do
      prettyPrint (App (Var 2) (Abs (V 1) (Abs Discard (Var 1)))) `shouldBe` "x2 \\x1 _ -> x1"
  describe "challenge4" $ do
    it "test1" $ do
      parseLetx "x1 (x2 x3)" `shouldBe` Just (App (Var 1) (App (Var 2) (Var 3)))
    it "test2" $ do
      parseLetx "x1 x2 x3" `shouldBe` Just (App (App (Var 1) (Var 2)) (Var 3))
    it "test3" $ do
      parseLetx "let x1 x3 = x2 in x1 x2" `shouldBe` Just (Let (V 1) (Abs (V 3) (Var 2)) (App (Var 1) (Var 2)))
    it "test4" $ do
      parseLetx "let x1 _ x3 = x3 in \\x3 -> x1 x3 x3" `shouldBe` Just (Let (V 1) (Abs Discard (Abs (V 3) (Var 3))) (Abs (V 3) (App (App (Var 1) (Var 3)) (Var 3))))
  describe "challenge5" $ do
    it "test Let" $ do
      let x1 = Let Discard (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))
      let x2 = LamApp (LamAbs 0 (LamAbs 1 (LamVar 1))) (LamAbs 0 (LamVar 0))
      letEnc x1 `shouldBe` x2
    it "test Let Scope Chain" $ do
      let x1 = Let (V 0) (Abs (V 1) (Var 1)) (Abs (V 2) (Var 0))
      let x2 = LamApp (LamAbs 0 (LamAbs 1 (LamVar 0))) (LamAbs 0 (LamVar 0))
      letEnc x1 `shouldBe` x2
    it "test fst" $ do
      let x1 = Fst (Pair (Abs (V 1) (Var 1)) (Abs Discard (Var 2)))
      let x2 = LamApp (LamAbs 0 (LamApp (LamApp (LamVar 0) (LamAbs 1 (LamVar 1))) (LamAbs 1 (LamVar 3)))) (LamAbs 0 (LamAbs 1 (LamVar 0)))
      letEnc x1 `shouldBe` x2
  describe "challenge6" $ do
    it "test1" $ do
      let x1 = compareRedn (Let (V 3) (Pair (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2))) (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2)))) (Fst (Var 3))) 10
      let x3 = Let (V 3) (Pair (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2))) (App (Abs (V 2) (Var 2)) (Abs (V 2) (Var 2)))) (Fst (Var 3))
      let x2 = (6, 8, 4, 6)
      x1 `shouldBe` x2
    it "test2" $ do
      let x1 = compareRedn (Let Discard (App (Abs (V 1) (Var 1)) (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))) (Snd (Pair (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))) (Abs (V 1) (Var 1))))) 10
      let x2 = (5, 7, 2, 4)
      x1 `shouldBe` x2
    it "test3" $ do
      let x1 = compareRedn (Let (V 2) (Let (V 1) (Abs (V 0) (App (Var 0) (Var 0))) (App (Var 1) (Var 1))) (Snd (Pair (Var 2) (Abs (V 1) (Var 1))))) 100
      let x2 = (100, 100, 2, 4)
      x1 `shouldBe` x2
