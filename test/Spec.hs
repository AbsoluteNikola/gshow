import Relude
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Relude.Unsafe (read)

main :: IO ()
main = defaultMain tests


check :: (Eq a, Show a, Read a, Showable a) => a -> Assertion
check x = read (show' x) @?= x

data R1 = R1 { a :: Int, b :: String }
  deriving (Eq, Show, Generic, Showable, Read)

data R3 = R3 { a :: Int, b :: String, c :: R1 }
  deriving (Eq, Show, Generic, Showable, Read)

data R2 = R2 Int String
  deriving (Eq, Show, Generic, Showable, Read)

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Number" $ check (1 :: Int)
  , testCase "String" $ check ("hello" :: String)
  , testCase "Tuple"  $ check ("hello" :: String , 1 :: Int, 2 :: Int)
  , testCase "List"   $ check [1 :: Int, 2, 3]
  , testCase "Bool T" $ check True
  , testCase "Bool F" $ check False
  , testCase "Record with accessors" $ check (R1 1 "hello")
  , testCase "Record without accessors" $ check (R2 1 "hello")
  , testCase "Nested Record without accessors" $ check (R3 1 "hello" (R1 2 "world"))
  ]
