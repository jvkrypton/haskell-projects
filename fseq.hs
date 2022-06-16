fseqverify :: [Char] -> [Char] -> Bool
fseqverify x y
 | null y == True = False  
 | head x == head y = fseq2 (tail x) (tail y)
 | head x /= head y = fseqverify x (tail y)
    where fseq2 x y
            | null x = True
            | head x == head y = fseq2 (tail x) (tail y)
            | otherwise = False

fseq :: [Char] -> [Char] -> Bool
fseq a b
 | length(a) <= 1 = False
 | otherwise = fseqroutine a b
    where fseqroutine x y
            | length x > length y = False
            | null x == True = True
            | elem (head x) y == True = fseqverify x y
            | elem (head x) y == False = False

-- Use: fseq "Chip" "Fish & Chips" | fseq "Chip" "Chin up"
