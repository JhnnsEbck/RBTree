module RBTree (RBTree(..), insert, inorder) where

-- Red-Black-Tree Datentyp und Funktionen
data Color = Red | Black -- Color ist entweder Red oder Black
data RBTree a = Empty | Node Color (RBTree a) a (RBTree a) -- initialisiert leeren Baum oder Knoten mit Farbe, linkem Kind, Wert und rechtem Kind

-- Einfügen von Elementen in den Baum
insert :: Ord a => a -> RBTree a -> RBTree a -- Ord a = a ist eine Instanz von Ord (Ordnung) -> garantiert, dass a vergleichbar ist
insert x tree = makeBlack (ins tree) -- ins = rekursive Funktion, die Elemente in den Baum einfügt; makeBlack = macht die Wurzel schwarz -> Wurzel nach dem Einfügen ist immer schwarz
  where
    ins Empty = Node Red Empty x Empty -- Wenn Baum leer (ganz am Anfang) -> neuer Knoten mit Farbe Red
    ins (Node color left value right) -- Wenn Baum nicht leer -> rekursiver Aufruf von ins
      | x < value = balance color (ins left) value right -- links einfügen
      | x > value = balance color left value (ins right) -- rechts einfügen
      | otherwise = Node color left value right -- Wenn x = value -> nichts einfügen (keine Wörter doppelt einfügen)
    makeBlack (Node _ left value right) = Node Black left value right -- wird aufgerufen, wenn ins fertig ist -> Wurzel schwarz machen
    makeBlack Empty = Empty -- Wenn Baum leer -> leere Liste
 
-- Balancing
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a -- in Funktion balance werden rot-rot Konflikte gelöst
balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d) -- links - links -> rotieren
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d) -- links - rechts -> rotieren
balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d) -- rechts - links -> rotieren
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d) -- recht - rechts -> rotieren
balance color left value right = Node color left value right -- Wenn kein rot-rot Konflikt -> nichts machen

-- Inorder Baum Traversierung (Rekursiv)
inorder :: RBTree a -> [a] -- erwartet einen Red-Black-Tree und gibt eine Liste von Werten zurück
inorder Empty = [] -- Diese Zeile muss hier stehen, weil RBTree a = Empty | Node Color (RBTree a) a (RBTree a) -> Empty ist ein Fall von RBTree a
inorder (Node _ left value right) = inorder left ++ [value] ++ inorder right -- _ = Farbe des Knotens; inorder traversiert den Baum in der Reihenfolge links - Wurzel - rechts
