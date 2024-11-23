module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha)
import RBTree (RBTree(..), insert, inorder)

-- Datei-Handling und Tokenisierung
tokenize :: T.Text -> [T.Text]
tokenize = T.words 
        . T.toLower 
        . T.map (\c -> if c == '\n' then ' ' else c) 
        . T.filter (\c -> isAlpha c || c == ' ' || c == '\n')


-- Tokenize text into words using a regex pattern
-- tokenize :: T.Text -> [T.Text]
-- tokenize text = map T.pack (text =~ "[a-zA-Z']+" :: [String])


-- Red-Black-Tree mit Wörtern füllen
buildTree :: [T.Text] -> RBTree T.Text
buildTree = foldr insert Empty

-- Main Funktion
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- TIO.readFile filePath
      let words = tokenize content
          tree = buildTree words
          uniqueSortedWords = inorder tree
      TIO.writeFile "output.txt" (T.unlines uniqueSortedWords)
      putStrLn "Output written to output.txt"
    _ -> putStrLn "Usage: <program> <path_to_text_file>"
