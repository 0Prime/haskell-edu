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
    it "test 1" $ do
      let x = Un 'a'
      let y = Un 'b'
      let z = Un 'c'
      concat3OC x y z `shouldBe` Bi 'a' 'b' (Un 'c')

    it "test 2" $ do
      let x = Bi 'a' 'b' (Un 'c')
      let y = Un 'd'
      let z = Un 'e'
      concat3OC x y z
        `shouldBe` Bi 'a' 'b' (Bi 'c' 'd' (Un 'e'))

    it "test 3" $ do
      let x = Un 'a'
      let y = Bi 'b' 'c' (Un 'd')
      let z = Un 'e'
      concat3OC x y z
        `shouldBe` Bi 'a' 'b' (Bi 'c' 'd' (Un 'e'))

    it "test 4" $ do
      let x = Un 'a'
      let y = Un 'b'
      let z = Bi 'c' 'd' (Un 'e')
      concat3OC x y z
        `shouldBe` Bi 'a' 'b' (Bi 'c' 'd' (Un 'e'))

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