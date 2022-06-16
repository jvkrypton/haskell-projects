elemPosition :: (Eq a) => a -> [a] -> [Int]
elemPosition ele tList 
 | null tList = []
 | otherwise = [ yi | (xi, yi) <- zip tList [0..], ( (xi == ele && xi `elem` (_selfDuplication tList)) || (xi == ele && xi `notElem` (_selfDuplication tList)) ) ] 
  where _selfDuplication xs = [x | (x, y) <- zip xs [0..], x `elem` (take y xs)]

-- Modify the qSort function seen in the classroom in such a way that it sorts the elements in descending order.
qsortDesc :: [Int] -> [Int]
qsortDesc [] = []
qsortDesc (x:xs)
 |xs == [] = [x]
 |otherwise = qsortDesc [y | y <- xs, y >= x] ++ [x] ++ qsortDesc [y | y <- xs, y < x]  

-- Utilização: qsortDesc [5,7,4,9,3,2,1,0]


-- Adapt the qSort function so that, in addition to sorting, it eliminates duplicate elements.
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs)
 |xs == [] = [x]
 |otherwise = qsort [y | y <- xs, y <= x, (length(elemPosition y xs) > 0 && (head (x:xs)) /= y )] ++ [x] ++
  qsort [y | y <- xs, y > x, (length(elemPosition y xs) > 0 && (head (x:xs)) /= y )] 

-- Utilização: qsort [5,7,4,9,3,2,5,1,2,0]
