import Test.Hspec
import Test.Hspec.Runner
  (runSpec, defaultConfig, evaluateSummary, configFormatter)
import Test.Hspec.Formatters (progress)

spec :: Spec
spec = do
  describe "main" $ do
    it "is evaluated" $
      "main = 0" `shouldBe` "main = 0"

main :: IO ()
main = hspec spec

hspecProgress :: Spec -> IO ()
hspecProgress spec =
  evaluateSummary
    =<< runSpec spec (defaultConfig {configFormatter = Just progress})

-- $> hspecProgress spec