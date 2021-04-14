module Stepic2.Step31Spec (spec) where

import Stepic2.Step31
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Except" $ do
    it "smoke test" $ do
      'A' `shouldBe` 'A'

  describe "withExcept" $ do
    it "applies function when is Left" $ do
      withExcept succ (Except (Left 'A') :: Except Char ())
        `shouldBe` Except (Left 'B')

    it "does nothing when is Right" $ do
      withExcept succ (Except (Right ()) :: Except Char ())
        `shouldBe` Except (Right ())
