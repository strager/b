import qualified Spec.B.Build

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Spec.B.Build" Spec.B.Build.spec
