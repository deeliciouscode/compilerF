import Test.Hspec
import Test.Hspec.Runner
  (runSpec, defaultConfig, evaluateSummary, configFormatter)
import Test.Hspec.Formatters (progress)
import Emulator
import DataStructures

spec :: Spec
spec = do
  describe "Addition" $ do
    it "can add 2 values" $
      emulateFromString "main = 210+210;" `shouldBe` RInt 420
  describe "Subtraction" $ do
    it "subtract one integer from another" $
      "1 + 2" `shouldBe` "1 + 2"
  describe "And" $ do
    it "true and true" $
      emulateFromString "main = true & true;" `shouldBe` RBool True
    it "false and true" $
      emulateFromString "main = false & true;" `shouldBe` RBool False
  describe "Or" $ do
    it "false or true" $
      emulateFromString "main = false | true;" `shouldBe` RBool True
    it "false or false" $
      emulateFromString "main = false | false;" `shouldBe` RBool False
  describe "Multiplication" $ do
    it "can multiply two values" $
      emulateFromString "main = 2 * 210;" `shouldBe` RInt 420
  describe "Division" $ do
    it "can divide two values" $
      emulateFromString "main = 138 / 2;" `shouldBe` RInt 69
  describe "Not" $ do
    it "can negate Bool values" $
      emulateFromString "main = not true;" `shouldBe` RBool False
  describe "Negate" $ do
    it "can negate Int values" $
      emulateFromString "main = -1;" `shouldBe` RInt (-1)
  describe "Equals" $ do
    it "check two arguments for equality" $
      emulateFromString "main = 1 == 1;" `shouldBe` RBool True
  describe "LessThan" $ do
    it "check if one argument is less than another" $
      emulateFromString "main = 1 < 2;" `shouldBe` RBool True
    it "check if one argument is less than another" $
      emulateFromString "main = 1 < 1;" `shouldBe` RBool False
  describe "If" $ do
    it "executes expressions based on true condition" $
      emulateFromString "main = if true then 1 else 0;" `shouldBe` RInt 1
    it "executes expressions based on false condition" $
      emulateFromString "main = if false then 0 else 1;" `shouldBe` RInt 1
  describe "Local definition" $ do
    it "contains local definitions and an expression" $
      emulateFromString "main = let a = 1; b = 3; c = a in a + b + c;" `shouldBe` RInt 5
    it "contains local definitions and an expression" $
      emulateFromString "main = let a = 1; b = a in a + b;" `shouldBe` RInt 2
  describe "Id Function" $ do
    it "returns self" $
      emulateFromString "f a = a; main = f 0;" `shouldBe` RInt 0
  describe "Function call" $ do
    it "calls defined function with arguments" $
      emulateFromString "f a b c = a; main = f 1 2 3;" `shouldBe` RInt 1
  describe "Function call with function as arguments" $ do
    it "calls defined function with arguments" $
      emulateFromString "f1 a b = b ;f a b = a; main = f (f1 1 2) 3;" `shouldBe` RInt 2
  describe "Nested Function call " $ do
    it "calls defined function inside another defined function" $
      emulateFromString "f a = a; g a b = a | b; main = g true (f false);" `shouldBe` RBool True
  describe "Testprog1" $ do
    it "" $
      emulateFromString "main = k1 0 1; k1 a b = b;" `shouldBe` RInt 1
  describe "Testprog2" $ do
    it "" $
      emulateFromString "main = f 0 1 2; f a b c = c;" `shouldBe` RInt 2
  describe "Testprog3" $ do
    it "" $
      emulateFromString "main = f 0 1 2; f a b c = k1 a b; k1 a b = b;" `shouldBe` RInt 1
  describe "Testprog4" $ do
    it "" $
      emulateFromString "main = k1 0 (k1 1 2); k1 a b = b;" `shouldBe` RInt 2
  describe "Testprog5" $ do
    it "" $
     emulateFromString "main = f 0 1 2; f a b c = c;" `shouldBe` RInt 2
  describe "Testprog6" $ do
    it "" $
      emulateFromString "main = f 0 1 2; k1 a b = b; f a b c = k1 a b;" `shouldBe` RInt 1
  describe "Testprog7" $ do
    it "" $
      emulateFromString "main = false == (not false);" `shouldBe` RBool False
  describe "Testprog8" $ do
    it "" $
      emulateFromString "main = let a = 1; b = 2; c = a in a + b + c;" `shouldBe` RInt 4
  describe "Testprog9" $ do
    it "" $
      emulateFromString "main = let a = 1; b = a; c = 2 in a + b + c;" `shouldBe` RInt 4
  describe "Testprog10" $ do
    it "" $
      emulateFromString "main = let a = 1; b = 2; c = b in a + b + c;" `shouldBe` RInt 5
  describe "Testprog11" $ do
    it "" $
      emulateFromString "main = let a = 1; b = 2; c = b in a + b + c;" `shouldBe` RInt 5
  describe "Testprog12" $ do
    it "" $
      emulateFromString "main = let a = 1; b = 2; c = (a + b) in if not (a == b) | (c / 2 == 0) then c else a;" `shouldBe` RInt 3
  describe "Testprog13" $ do
    it "" $
      emulateFromString "main = f 2; f a = let b = 3 in a + b;" `shouldBe` RInt 5
  describe "Testprog14" $ do
    it "" $
      emulateFromString "main = f 2; f a = let b = 3; c = 5 in a + b;" `shouldBe` RInt 5
  describe "Testprog15" $ do
    it "" $
      emulateFromString "main = f 2; f a = let b = 3; c = 5 in a + b;" `shouldBe` RInt 5
  describe "Testprog16" $ do
    it "" $
      emulateFromString "a = if false then 0 else -1; main = a;" `shouldBe` RInt (-1)

main :: IO ()
main = hspec spec

hspecProgress :: Spec -> IO ()
hspecProgress spec =
  evaluateSummary
    =<< runSpec spec (defaultConfig {configFormatter = Just progress})

-- $> hspecProgress spec
