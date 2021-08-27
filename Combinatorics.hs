module Combinatorics where

sigma :: Int -> Int -> (Int -> Int) -> Int
sigma mini maks f = sum $ map f [mini..maks]

phi :: Int -> Int -> (Int -> Int) -> Int
phi mini maks f = product $ map f [mini..maks]

factorial :: Int -> Int
factorial x =  phi 1 x id

choose :: Int -> Int -> Int
choose n k 
    | n < 0 = 0
    | k < 0 = 0
    | otherwise = div (phi (n-k+1) n id) (factorial k)

-- PMC = Per Mine Count
totalCasePMC' :: Int -> Int -> Int -> Int -> Int -> Int 
              -> Int
totalCasePMC' a b x p q i = 
    (choose a (p-i)) *
    (choose x i) *
    (choose b (q-i))

totalCase' :: Int -> Int -> Int -> Int -> Int
           -> Int
totalCase' a b x p q  = 
    sigma 0 (max p q) (totalCasePMC' a b x p q)

overlapCasePMC' :: Int -> Int -> Int -> Int -> Int -> Int 
                -> Int
overlapCasePMC' a b x p q i = 
    (choose a (p-i)) *
    (choose (x-1) (i-1)) *
    (choose b (q-i))

overlapCase' :: Int -> Int -> Int -> Int -> Int
             -> Int
overlapCase' a b x p q =
    sigma 0 (max p q) (overlapCasePMC' a b x p q)

firstCasePMC' :: Int -> Int -> Int -> Int -> Int -> Int
              -> Int
firstCasePMC' a b x p q i = 
    (choose (a-1) (p-i-1)) *
    (choose x i) *
    (choose b (q-i))

-- somehow this can't work
outerCase' :: Int -> Int -> Int -> Int -> Int
           -> (Int, Int)
outerCase' a b x p q = (x,y)
    where
        x = sigma 0 (max p q) (firstCasePMC' a b x p q)
        y = sigma 0 (max p q) (firstCasePMC' b a x q p)

outerCase1 :: Int -> Int -> Int -> Int -> Int
           -> Int
outerCase1 a b x p q = 
    sigma 0 (max p q) (firstCasePMC' a b x p q)

outerCase2 :: Int -> Int -> Int -> Int -> Int
           -> Int
outerCase2 a b x p q = 
    sigma 0 (max p q) (firstCasePMC' b a x q p)

-- proper notation
totalCase :: (Int, Int) -> (Int, Int) -> Int
          -> Int
totalCase (m1, s1) (m2, s2) so
    = totalCase' (s1-so) (s2-so) so m1 m2 

overlapCase :: (Int, Int) -> (Int, Int) -> Int
            -> Int
overlapCase (m1, s1) (m2, s2) so
    = overlapCase' (s1-so) (s2-so) so m1 m2 

outerCase :: (Int, Int) -> (Int, Int) -> Int
          -> (Int, Int)
outerCase (m1, s1) (m2, s2) so
    = (outerCase1 (s1-so) (s2-so) so m1 m2,
       outerCase2 (s1-so) (s2-so) so m1 m2)

main :: IO()
main = do
    print (sigma 1 5 id)
    print (phi 1 5 id)
    print (choose 1 3)
    print (outerCase' 1 1 1 1 1)
