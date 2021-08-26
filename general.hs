import Combinatorics (sigma, phi, factorial, choose)
import Data.Bits ((.|.))

-- Bitwise operation
-- 0 means there's mine
-- 1 means there's not
-- Only pick minimum where there is
minimumMine :: Int -> [Int]
            -> Int
minimumMine currentCon [] = maxBound
minimumMine currentCon mineCount = 
    let 
        prevMinimumMine = minimumMine (div currentCon 2)
                                      (tail mineCount)
        currentMine = head mineCount
    in
        if mod currentCon 2 == 0 then 
            if currentMine < prevMinimumMine then currentMine
                else prevMinimumMine
            else prevMinimumMine

-- Add Mines to all containers that are related to current con
addMines :: Int -> [Int] -> Int -> Int
         -> [Int]
addMines currentMine [] currentCon currentIdx = []
addMines currentMine mineInCon currentCon currentIdx = 
    if (currentCon+currentIdx) .|. currentCon == 
       (currentCon+currentIdx) then              -- bitwise magic
       ((head mineInCon) + currentMine) :
        addMines currentMine
                 (tail mineInCon)
                 currentCon 
                 (currentIdx+1) 
    else
        (head mineInCon) : addMines currentMine
                                    (tail mineInCon)
                                    currentCon
                                    (currentIdx+1)

-- Count all zeroes in a binary number
countZero :: Int -> Int
          -> Int
countZero 0 val = 0
countZero length val = 
    (mod ((mod val 2) + 1) 2) + countZero (length-1) (div val 2)

-- Count all cases for the next container
totalNextCon :: [Int] -> [Int] -> [Int] -> Int -> Int
             -> Int
totalNextCon mineCount 
             containers 
             mineInCon
             currentCon
             currentMine 
    = (choose (head containers) currentMine)
      * (totalCase mineCount
                   (tail containers)
                   (tail $ addMines currentMine
                                    mineInCon
                                    currentCon
                                    0)
                   (currentCon+1))

-- Brute force all possibility for filling each containers
totalCase :: [Int] -> [Int] -> [Int] -> Int
          -> Int
totalCase mineCount [] [] currentCon = 1
totalCase mineCount containers [] currentCon 
    = totalCase mineCount 
                containers 
                (replicate (length containers) 0)
                currentCon
totalCase mineCount containers mineInCon currentCon
    | countZero (length mineCount) currentCon == 1 
        = totalNextCon mineCount
                       containers
                       mineInCon
                       currentCon
                       ((minimumMine currentCon mineCount)
                        - (head mineInCon))
    | otherwise = sigma 0 
                        ((minimumMine currentCon mineCount)
                         - (head mineInCon))
                        (totalNextCon mineCount
                                      containers
                                      mineInCon
                                      currentCon)

main = do
    print (minimumMine 3 [8,2,10])
    print (addMines 3 [5,5,5,5] 2 0)
    print (countZero 2 0)
    print (totalCase [1] [3] [] 0)
    print (totalCase [1,2] [2,1,0] [] 0)
    print (totalCase [1,2,1] [1,1,0,3,1,0,3] [] 0)
