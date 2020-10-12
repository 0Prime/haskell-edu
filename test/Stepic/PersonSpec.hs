module Stepic.PersonSpec (spec) where

import Stepic.Person
import Test.Hspec

spec :: Spec
spec = parallel $ do
  context "invalid input" $ do
    describe "ParsingError" $ do
      let test msg input = it msg $ do
            parsePerson input `shouldBe` Left ParsingError

      "fails on empty string"
        `test` ""

      "fails on empty str field value"
        `test` "firstName = \nlastName = Bar\nage = 42"

      "fails on empty num field value"
        `test` "firstName = Foo\nlastName = Bar\nage = "

      "fails on no spaces arount '='"
        `test` "foo=bar\nfirstName = Meh"

    describe "IncompleteDataError" $ do
      let msg `on` input = it msg $ do
            parsePerson input `shouldBe` Left IncompleteDataError

      "fails on incomplete data"
        `on` "firstName = foo\nlastName = bar"

      "fails on fields with extra spaces"
        `on` " firstName = foo\nlastName = bar\nage = 42 "

      "fails on fields with extra spaces and non numeric age"
        `on` " firstName = John\nlastName = Connor\nage = 2f8 "

    describe "IncorrectDataError" $ do
      it "fails when age is not a number" $ do
        parsePerson "firstName = John\nlastName = Connor\nage = as30"
          `shouldBe` Left (IncorrectDataError "as30")

  context "valid input" $ do
    let is input expected = it "" $ do
          parsePerson input `shouldBe` Right expected

    "firstName = Foo\nlastName = Bar\nage = 42"
      `is` Person {firstName = "Foo", lastName = "Bar", age = 42}

    "lastName = Bar\nfirstName = Foo\nage = 42"
      `is` Person {firstName = "Foo", lastName = "Bar", age = 42}

    "firstName = John\nlastName = Connor\nfoo = bar\nage = 30"
      `is` Person {firstName = "John", lastName = "Connor", age = 30}

    "firstName = Barbarian\nlastName = Conn On\nage = 30"
      `is` Person {firstName = "Barbarian", lastName = "Conn On", age = 30}

    "firstName = John\nlastName = Con=nor\nage = 30"
      `is` Person {firstName = "John", lastName = "Con=nor", age = 30}
