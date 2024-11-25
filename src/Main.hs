module Main where

import System.Environment (getArgs) -- getArgs gibt die Argumente zurück, die dem Programm asu dem Terminal übergeben wurden
import qualified Data.Text as T -- Data.Text = Bibliothek, die Textverarbeitung ermöglicht
import qualified Data.Text.IO as TIO -- Data.Text.IO = Bibliothek für lesen und schreiben von Text
import Data.Char (isAlpha)
import RBTree (RBTree(..), insert, inorder) -- importiert die Funktionen aus dem File RBTree.hs
import Data.Time (getCurrentTime, diffUTCTime)

-- Datei-Handling und Tokenisierung
tokenize :: T.Text -> [T.Text] -- Funktion tokenize bekommt einen Text und gibt eine Liste von Texten zurück
tokenize = T.words -- zerlegt Text in Wörter die durch Whitespaces (in dem Fall: Leerzeichen) getrennt sind
        . T.toLower 
        . T.map (\c -> if c == '\n' then ' ' else c) -- ersetzt Zeilenumbrüche durch Leerzeichen
        . T.filter (\c -> isAlpha c || c == ' ' || c == '\n') -- filtert alle Zeichen, die keine Buchstaben, Leerzeichen oder Zeilenumbrüche sind

-- Wörter mit Red-Black-Tree sortieren
rbTreeSort :: [T.Text] -> RBTree T.Text -- erhält Liste von T.Text (Wörtern) und gibt einen Red-Black-Tree zurück 
rbTreeSort = foldr insert Empty
-- mit folder wird durch die Liste von Wörtern iteriert 
-- mit insert (aus RBTree.hs) wird jedes Wort in den Red-Black-Tree eingefügt
-- mit Empty wird ein leerer Anfangsbaum initialisiert

-- Main Funktion
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- TIO.readFile filePath -- liest mit TIO.readFile den Inhalt von filePath (der mit getArgs aus dem Terminal übergeben wurde) in content ein
      let words = tokenize content -- tokenisierung über Funktion "tokenize"
          tree = rbTreeSort words -- sortierung über Funktion "buildTree"
          uniqueSortedWords = inorder tree -- traversiert den Baum und gibt die Wörter in sortierter Reihenfolge als Liste zurück
      TIO.writeFile "output.txt" (T.unlines uniqueSortedWords)
      putStrLn "List of unique words in given text written to output.txt"
    _ -> putStrLn "Usage: <program> <path_to_text_file>"
