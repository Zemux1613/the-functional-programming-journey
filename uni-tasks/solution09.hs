module Solution09 where

-- Possible manual definition of `head`, we'll just use the standard function.
--head :: [a] -> a
--head list = case list of
--  x:_ -> x

heads :: [[a]] -> [a]
heads = map head

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (n:ns) (h:hs) =
  n == h && isPrefix ns hs

isSublist :: Eq a => [a] -> [a] -> Bool
isSublist [] _ = True
isSublist _ [] = False
isSublist needle haystack@(_:hs) =
  isPrefix needle haystack || isSublist needle hs

revrev :: [[a]] -> [[a]]
revrev = reverse . map reverse

tokenize :: Eq a => [a] -> [a] -> [[a]]
tokenize _ [] = []
tokenize match string =
  if hd == [] then tl' else hd:tl'
  where
    (hd, tl) = span (not . flip elem match) string
    tl' = tokenize match $ drop 1 tl -- remove elment we just matched
  

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort l =
  let (a, b) = split l in
  merge (mergeSort a) (mergeSort b)
  where
    split :: [a] -> ([a], [a])
    split [] = ([], [])
    split [x] = ([x], [])
    split (x1:x2:xs) = let (x1s, x2s) = split xs in (x1:x1s, x2:x2s)

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] bs = bs
    merge as [] = as
    merge (a:as) (b:bs) =
      if a < b then a:(merge as (b:bs)) else b:(merge (a:as) bs)
