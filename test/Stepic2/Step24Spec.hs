module Stepic2.Step24Spec (spec) where

import Stepic2.Step14
import Stepic2.Step23
import Stepic2.Step24
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "PrsE" $ do
    describe "is Monad" $ do
      let testHelper = runPrsE $ do
            a <- charE 'A'
            b <- charE 'B'
            return (a, b)

      it "test 1" $ do
        testHelper "ABC" `shouldBe` Right (('A', 'B'), "C")

      it "test 2" $ do
        testHelper "ACD" `shouldBe` Left "unexpected C"

      it "test 3" $ do
        testHelper "BCD" `shouldBe` Left "unexpected B"

  describe "concat3OC" $ do
    let unX = Un "x1"
    let biX = Bi "x1" "x2" (Un "x3")
    let unY = Un "y1"
    let biY = Bi "y1" "y2" (Un "y3")
    let unZ = Un "z1"
    let biZ = Bi "z1" "z2" (Un "z3")

    it "test 1" $ do
      concat3OC unX unY unZ `shouldBe` Bi "x1" "y1" (Un "z1")

    it "test 2" $ do
      concat3OC biX unY unZ
        `shouldBe` Bi "x1" "x2" (Bi "x3" "y1" (Un "z1"))

    it "test 3" $ do
      concat3OC unX biY unZ
        `shouldBe` Bi "x1" "y1" (Bi "y2" "y3" (Un "z1"))

    it "test 4" $ do
      concat3OC unX unY biZ
        `shouldBe` Bi "x1" "y1" (Bi "z1" "z2" (Un "z3"))

    it "test 5" $ do
      let tst1 = Bi 'a' 'b' (Un 'c')
      let tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
      let tst3 = Bi 'i' 'j' (Un 'k')
      concat3OC tst1 tst2 tst3
        `shouldBe` Bi
          'a'
          'b'
          ( Bi
              'c'
              'd'
              ( Bi
                  'e'
                  'f'
                  (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))
              )
          )

  describe "concatOC" $ do
    it "test 1" $ do
      concatOC (Un (Un 42)) `shouldBe` Un 42

    it "test 2" $ do
      let a = Bi 'a' 'b' (Un 'c')
      let b = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
      let c = Bi 'i' 'j' (Un 'k')
      concatOC (Bi a b (Un c))
        `shouldBe` Bi
          'a'
          'b'
          ( Bi
              'c'
              'd'
              ( Bi
                  'e'
                  'f'
                  ( Bi 'g' 'h' (Bi 'i' 'j' (Un 'k'))
                  )
              )
          )

  describe "Oddc" $ do
    describe "is Applicative & Monad" $ do
      let tst1 = Bi 10 20 (Un 30)
      let tst2 = Bi 1 2 (Bi 3 4 (Un 5))

      it "test 1" $ do
        (do x <- tst1; y <- tst2; return (x + y))
          `shouldBe` Bi
            11
            12
            ( Bi
                13
                14
                ( Bi
                    15
                    21
                    ( Bi
                        22
                        23
                        (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35))))
                    )
                )
            )

      it "test 2" $ do
        (do x <- tst2; y <- tst1; return (x + y))
          `shouldBe` Bi
            11
            21
            ( Bi
                31
                12
                ( Bi
                    22
                    32
                    ( Bi
                        13
                        23
                        (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35))))
                    )
                )
            )