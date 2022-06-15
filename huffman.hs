module Huffman where
    import qualified Data.List (sortBy)
    import Text.Printf ( printf )

    
    data Tree a b = Branch b (Tree a b) (Tree a b) | Leaf a 
    -- Cred: https://stackoverflow.com/users/1532460/awesoon
    instance (Show a, Show b) => Show (Tree a b) where
        show = showAtLevel 0 where
            showAtLevel l (Leaf x) = addSpace l ++ show x
            showAtLevel l (Branch x lt rt) = printf "%s%s\n%s\n%s" (addSpace l) (show x) (showAtLevel (l + 1) lt) (showAtLevel (l + 1) rt)
            addSpace = flip replicate '\t'
    
    --instance Show Bool => Show [Bool] where
     --   show b[] = map show b

    sample :: [Char]
    sample = "the quick brown fox jumps over the lazy dog\
    \this is a sample text that we will use when we build\
    \up a table we will only handle lower case letters and\
    \no punctuation symbols the frequency will of course not\
    \represent english but it is probably not that far off"
    encSample :: [Char]
    encSample = "this is something that we should encode"
    
    lookupLeaf :: Char -> [Tree (Char, Int) Int] -> Maybe Int
    lookupLeaf k [] = Nothing
    lookupLeaf k (Leaf (l,f):tail) 
        | k == l = Just f
        | otherwise = lookupLeaf k tail 
    lookupLeaf k (Branch f l r:tail) = lookupLeaf k tail -- Should never match when used in this program context
    
    ordIns :: Tree (Char, Int) Int -> [Tree (Char, Int) Int] -> [Tree (Char, Int) Int]
    ordIns t [] = [t]
    ordIns (Branch f1 l r) (Leaf (k,f2):tail)
        | f1 > f2 = Leaf (k,f2):ordIns (Branch f1 l r) tail
        | otherwise = Branch f1 l r:tail
    ordIns (Branch f1 l1 r1) (Branch f2 l2 r2:tail)
        | f1 > f2 = Branch f2 l2 r2:ordIns (Branch f1 l1 r1) tail
        | otherwise = Branch f1 l1 r1:tail
    ordIns (Leaf (k1,f1)) (Leaf (k2,f2):tail)
        | f1 > f2 = Leaf (k2,f2):ordIns (Leaf (k1,f1)) tail
        | otherwise = Leaf (k1,f1):tail
    ordIns (Leaf (k1,f1)) (Branch f2 l2 r2:tail)
        | f1 > f2 = Branch f2 l2 r2:ordIns (Leaf (k1,f1)) tail
        | otherwise = Leaf (k1,f1):tail

    insertLeaf :: (Char, Int) -> [Tree (Char, Int) Int] -> [Tree (Char, Int) Int]
    insertLeaf t [] = [Leaf t]
    insertLeaf (k1,v1) (Leaf (k2,v2):tail)
        | k1 == k2 = Leaf (k1,v1):tail
        | otherwise = Leaf (k2,v2):insertLeaf (k1, v1) tail
    insertLeaf (k1,v1) (Branch f l r:tail) = Branch f l r:insertLeaf (k1, v1) tail -- Should never match when used in this program context

    createFrequencyLeafList :: [Char] -> [Tree (Char, Int) Int] -> [Tree (Char, Int) Int]
    createFrequencyLeafList [] accLeaves = sortByFrequency accLeaves
    createFrequencyLeafList (head:tail) accLeaves =
        case lookupLeaf head accLeaves of
            Nothing -> createFrequencyLeafList tail (insertLeaf (head, 1) accLeaves)
            Just v -> createFrequencyLeafList tail (insertLeaf (head, v+1) accLeaves)
    
    cmpTree :: Ord a1 => Tree (a2, a1) a1 -> Tree (a3, a1) a1 -> Ordering
    cmpTree (Leaf (_,f1)) (Leaf (_,f2)) = compare f1 f2
    cmpTree (Branch f1 _ _) (Leaf (_,f2)) = compare f1 f2
    cmpTree (Leaf (_,f1)) (Branch f2 _ _) = compare f1 f2
    cmpTree (Branch f1 _ _) (Branch f2 _ _) = compare f1 f2
    sortByFrequency :: [Tree (Char, Int) Int] -> [Tree (Char, Int) Int]
    sortByFrequency = Data.List.sortBy cmpTree

    huffmanEncTree :: [Tree (Char,Int) Int] -> [Tree (Char,Int) Int]
    huffmanEncTree ((Leaf (c1,f1)):(Leaf (c2,f2)):tail) = 
        huffmanEncTree (ordIns (Branch (f1+f2) (Leaf (c1,f1)) (Leaf (c2,f2))) tail)
    huffmanEncTree ((Leaf (c1,f1)):(Branch f2 l r):tail) = 
        huffmanEncTree (ordIns (Branch (f1+f2) (Leaf (c1,f1)) (Branch f2 l r)) tail)
    huffmanEncTree ((Branch f1 l r):(Leaf (c2,f2)):tail) = 
        huffmanEncTree (ordIns (Branch (f1+f2) (Branch f2 l r) (Leaf (c2,f2))) tail)
    huffmanEncTree ((Branch f1 l1 r1):(Branch f2 l2 r2):tail) = 
        huffmanEncTree (ordIns (Branch (f1+f2) (Branch f1 l1 r1) (Branch f2 l2 r2)) tail)
    huffmanEncTree f = f

    rev :: Foldable t => t a -> [a] -> [a]
    rev tail acc = foldl (flip (:)) acc tail

    --encodeTree :: Tree (Char,Int) Int [Bool] -> [Bool]
    --encodeTree :: Tree (a, b1) b2 -> [Bool] -> [(Char,Bool)]
    --encodeTree :: Tree (a, b1) b2 -> [Bool] -> [(a, [Bool] -> [Bool])]
    encodeTree :: Tree (a, b1) b2 -> [Bool] -> [(a, [Bool])]
    encodeTree (Leaf (c,f)) acc = [(c,rev acc [])]
    encodeTree (Branch f l r) acc = encodeTree l (False:acc) ++ encodeTree r (True:acc)
    