elemPosition :: (Eq a) => a -> [a] -> [Int]
elemPosition ele tList 
 | null tList = []
 | otherwise = [ yi | (xi, yi) <- zip tList [0..], ( (xi == ele && xi `elem` (_selfDuplication tList)) || (xi == ele && xi `notElem` (_selfDuplication tList)) ) ] 
  where _selfDuplication xs = [x | (x, y) <- zip xs [0..], x `elem` (take y xs)]


-- Removes elements from one list contained in another list.
eraseElmContainsList :: [Int] -> [Int] -> [Int]
eraseElmContainsList elm lst
 | null lst = []
 | otherwise = if ((head lst) `elem` elm) then
         eraseElmContainsList elm (tail lst) 
     else  
        (head lst) : eraseElmContainsList elm (tail lst)


-- List understanding
getDuplicatedsWithListComprehension :: [Int] -> [Int]
getDuplicatedsWithListComprehension xs = [x | (x, y) <- zip xs [0..], x `elem` (take y xs)]


-- No understanding of lists
getDuplicatedsRecursive :: [Int] -> [Int]
getDuplicatedsRecursive [] = []
getDuplicatedsRecursive (elem:elemxs) = if ( length (elemPosition elem elemxs) > 0) then 
        elem : (getDuplicatedsRecursive elemxs) 
    else
         (getDuplicatedsRecursive elemxs)


-- Function
unique :: [Int] -> [Int]
unique tlist = eraseElmContainsList (getDuplicatedsWithListComprehension tlist) tlist

uniqueRecursive :: [Int] -> [Int]
uniqueRecursive tlist = eraseElmContainsList (getDuplicatedsRecursive tlist) tlist

-- Returns the list of elements of xs which occur exactly once. With list comprehension and without.
-- Use: unique [4,2,1,3,2,3] , uniqueRecursive [4,2,1,3,2,3] 
