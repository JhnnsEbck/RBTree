import Test.HUnit

tokenize :: Test
testTokenize = TestCase $
    assertEqual "Tokenize a simple sentence"
    ["hello", "world"]
    (tokenize (T.pack "Hello, world!"))


main :: IO ()
main = runTestTT test_reverse

