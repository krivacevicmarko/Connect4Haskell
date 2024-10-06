module Module.Rose (Rose(..), size, height, leavesCount, leaves, elemsOnDepth, foldRoseR, foldRoseL, generateRose) where

data Rose a = Node a [Rose a] deriving (Eq,Show)

{-a) size - vraća broj čvorova stabla
height - računa visinu stabla, odnosno najdužu putanju (broj grana) od korena do lista
-}

size :: Rose a -> Int
size (Node _ children) = 1 + sum (map size children)

size2 :: Rose a -> Int
size2 (Node x []) = 1
size2 (Node _ children) = 1 + foldl (\acc r -> acc + size r) 0 children

height :: Rose a -> Int
height (Node _ [] ) = 1
height (Node _ children) = 1 + maximum (map height children)  -- moze i fmap height

{-b) leavesCount - vraća broj listova,
leaves - vraća listu koja sadrži vrednosti svih listova stabla-}

leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node _ children) = sum (map leavesCount children)

leavesCount2 :: Rose a -> Int
leavesCount2 (Node _ []) = 1
leavesCount2 (Node _ children) = foldl (\acc r -> acc + leavesCount2 r) 0 children

leaves :: Rose a -> [a]
leaves (Node a []) = [a]
leaves (Node _ children) = concatMap leaves children

leaves2 :: Rose a -> [a]
leaves2 (Node x []) = [x]
leaves2 (Node _ children) = foldr (\r acc -> leaves2 r ++ acc) [] children

{-c) elemsOnDepth - vraća vrednosti svih elemenat na određenoj dubini-}

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node a _) = [a]
elemsOnDepth n (Node _ children) = concatMap (elemsOnDepth (n-1)) children

{-d) instancirati tipsku klasu Functor za tip podataka Rose-}

instance Functor Rose where
    fmap f (Node x []) = Node (f x) []
    fmap f (Node x children) = Node (f x) (map (fmap f) children)

{-e) napisati funkciju foldRose koja izršava fold (levi ili desni)-}

foldRoseR :: (a->b->b) -> b -> Rose a -> b
foldRoseR f acc (Node x []) = f x acc
foldRoseR f acc (Node x children) = f x (foldr (flip (foldRoseR f)) acc children)

foldRoseL :: (b->a->b) -> b -> Rose a -> b
foldRoseL f acc (Node x []) = f acc x
foldRoseL f acc (Node x xs) = (foldl . foldRoseL) f (f acc x) xs

{-f) generateRose-}

generateRose :: (a -> [a]) -> Int -> a -> Rose a
generateRose f 0 x = Node x []
generateRose f n x = Node x (map (generateRose f (n-1)) (f x))

-- foldRoseL (+) 0 x
-- foldRoseE (+) 0 x

-- generateRose (\ x -> [x+1,x+2,x+3]) 1 2
-- generateRose (\x -> [x,-x,x*x]) 2 2
-- generateRose (\x -> [x+1,x*x*x]) 3 3

x = Node 1 [Node 2 [],Node 3 [],Node 4 []]
y = Node 5 [Node 4 [], Node 7 [Node 1 [], Node 2 []], Node 8 [Node 9 [Node 10 [Node 11 [Node 12 []]]]]]
z = Node 5 [Node 4 [Node 19 [Node 20 [Node 58 [],Node 59 [],Node 60 [Node 50 [Node 100 [Node 200 []]]]]]], Node 7 [Node 1 [], Node 2 []]]
w = Node 20 [Node 1 [Node 11 [Node 111 []], Node 12 [Node 121 []]],Node 2 [Node 21 [Node 211 []], Node 22 [Node 221 []]], Node 3 [Node 31 [Node 311 []], Node 32 [Node 321 []]], Node 4 [Node 41 [Node 411 []], Node 42 [Node 421 []]], Node 5 [Node 51 [Node 511 []], Node 52 [Node 521 []]]]




brojNeparnih :: Rose Int -> Int
brojNeparnih rose = foldRoseR (\x acc -> (if odd x then 1 else 0) + acc) 0 rose

brojParnih :: Rose Int -> Int
brojParnih rose = foldRoseR (\x acc -> (if even x then 1 else 0) + acc) 0 rose
