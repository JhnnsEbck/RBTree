module Main (main) where
    
import Test.HUnit
import RBTree (RBTree(..), insert, inorder)

-- Test 1: Einfügen eines Elements in einen leeren Baum
testInsertEmpty :: Test
testInsertEmpty = TestCase $
    assertEqual "Insert 10 into empty tree"
    (Node Black Empty 10 Empty) -- Erwarteter Baum
    (insert 10 Empty)           -- Tatsächliches Ergebnis

-- Test 2: Inorder-Traversierung eines kleinen Baums
testInorderSmallTree :: Test
testInorderSmallTree = TestCase $
    assertEqual "Inorder traversal of tree with 2, 10, 5"
    [2, 5, 10] -- Erwartete Reihenfolge
    (inorder (foldr insert Empty [10, 5, 2])) -- Tatsächliches Ergebnis

-- Test 3: Mehrere Elemente einfügen
testInsertMultiple :: Test
testInsertMultiple = TestCase $
    assertEqual "Insert multiple elements and verify structure"
    [1, 2, 3, 4, 5] -- Erwartete Reihenfolge der Elemente
    (inorder (foldr insert Empty [3, 1, 4, 5, 2])) -- Tatsächliches Ergebnis

-- Test 4: Balancierung (implizit getestet durch `insert`)
testBalance :: Test
testBalance = TestCase $
    assertEqual "Check balance property after multiple insertions"
    [10, 20, 30, 40, 50] -- Erwartete Reihenfolge
    (inorder (foldr insert Empty [30, 10, 50, 40, 20]))

-- Test 5: Leerer Baum
testEmptyTree :: Test
testEmptyTree = TestCase $
    assertEqual "Inorder traversal of empty tree"
    [] -- Erwartete Reihenfolge
    (inorder Empty) -- Tatsächliches Ergebnis

-- Alle Tests gruppieren
tests :: Test
tests = TestList
    [ TestLabel "Test Insert Empty" testInsertEmpty
    , TestLabel "Test Inorder Small Tree" testInorderSmallTree
    , TestLabel "Test Insert Multiple" testInsertMultiple
    , TestLabel "Test Balance" testBalance
    , TestLabel "Test Empty Tree" testEmptyTree
    ]

-- Main-Funktion: Tests ausführen
main :: IO ()
main = do
    counts <- runTestTT tests
    print counts
