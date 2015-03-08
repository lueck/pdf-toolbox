{-# LANGUAGE OverloadedStrings #-}

module Test.Object.Builder
(
  spec,
)
where

import Pdf.Toolbox.Core.Object.Types
import Pdf.Toolbox.Core.Object.Builder

import qualified Data.ByteString.Builder as Builder
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Test.Hspec

spec :: Spec
spec = describe "Object.Builder" $ do
  buildBoolSpec
  buildStringSpec
  buildNameSpec
  buildNumberSpec
  buildArraySpec
  buildDictSpec

buildBoolSpec :: Spec
buildBoolSpec = describe "buildBool" $ do
  it "should build 'true' for True" $ do
    let res = buildBool True 
    Builder.toLazyByteString res `shouldBe` "true"

  it "should build 'false' for False" $ do
    let res = buildBool False
    Builder.toLazyByteString res `shouldBe` "false"

buildStringSpec :: Spec
buildStringSpec = describe "buildString" $ do
  it "should produce literal string when all chars are printable" $ do
    let res = buildString "hello"
    Builder.toLazyByteString res `shouldBe` "(hello)"

  it "should produce hex string when there are not printable chars" $ do
    let res = buildString "\NUL\255"
    Builder.toLazyByteString res `shouldBe` "<00ff>"

  it "should escape special chars" $ do
    let res = buildString "()\\"
    Builder.toLazyByteString res `shouldBe` "(\\(\\)\\\\)"

buildNameSpec :: Spec
buildNameSpec = describe "buildName" $ do
  it "should build a name" $ do
    let res = buildName "hello"
    Builder.toLazyByteString res `shouldBe` "/hello"

buildNumberSpec :: Spec
buildNumberSpec = describe "buildNumber" $ do
  it "should build int" $ do
    let res = buildNumber 42
    Builder.toLazyByteString res `shouldBe` "42"

  it "should build float" $ do
    let res = buildNumber 42.4
    Builder.toLazyByteString res `shouldBe` "42.4"

buildArraySpec :: Spec
buildArraySpec = describe "buildArray" $ do
  it "should build an array" $ do
    let res = buildArray (Vector.fromList [ONumber 42, OBoolean False])
    Builder.toLazyByteString res `shouldBe` "[42 false]"

  it "should build empty array" $ do
    let res = buildArray Vector.empty
    Builder.toLazyByteString res `shouldBe` "[]"

buildDictSpec :: Spec
buildDictSpec = describe "buildDict" $ do
  it "should build a dictionary" $ do
    let res = buildDict (HashMap.fromList [("hello", OBoolean False)])
    Builder.toLazyByteString res `shouldBe` "<</hello false>>"