module Utility (tokenize, rbTreeSort) where

import qualified Data.Text as T
import Data.Char (isAlpha)
import RBTree (RBTree(..), insert)

-- Datei-Handling und Tokenisierung
tokenize :: T.Text -> [T.Text]
tokenize = T.words 
        . T.toLower 
        . T.map (\c -> if c == '\n' then ' ' else c) 
        . T.filter (\c -> isAlpha c || c == ' ' || c == '\n')

-- Wörter mit Red-Black-Tree sortieren
rbTreeSort :: [T.Text] -> RBTree T.Text
rbTreeSort = foldr insert Empty
