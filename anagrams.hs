getUnique :: [String] -> [String]
getUnique [] = []
getUnique [x] = [x]
getUnique (x:xs) = x : [ y | y <- getUnique (xs), y /=x ]

getAnagrams :: String -> [String]
getAnagrams [] = return []
getAnagrams (x:xs) = getAnagrams(xs) >>= self_addItem(x)
  where
    self_addItem k []     = [[k]]
    self_addItem k (y:ys) = [k:y:ys] ++ ( map (y:) (self_addItem(k) ys) )

fmtAnagrams :: String -> String
fmtAnagrams [] = ""
fmtAnagrams xs = self_getElements( getUnique( getAnagrams(xs) ) ) ++ " : " ++ xs ++ "\n"
  where
    self_getElements [] = ""
    self_getElements (x:xs) = x ++ if ( length(xs) > 0 ) then ", " ++ self_getElements(xs) else self_getElements(xs)

anagrams :: Int -> [String] -> String
anagrams len xs
  | null xs = error "Empty"
  | length(xs) > 0 = self_getAnagrams ( [ y | y <- xs, length(y) == len ] )
    where 
      self_getAnagrams [] = ""
      self_getAnagrams list 
        | length(list) > 0 = fmtAnagrams( head(list) ) ++ self_getAnagrams( tail(list) )

-- Use: putStr( anagrams 4 ["word", "fast", "slow", "bigger", "actor", "end", "this"] )
