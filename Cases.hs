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
       (currentCon+currentIdx) then         -- bitwise magic
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
      * (totalCase' mineCount
                   (tail containers)
                   (tail $ addMines currentMine
                                    mineInCon
                                    currentCon
                                    0)
                   (currentCon+1))

-- Brute force all possibility for filling each containers
-- length mineInCon == containers
-- length containers == (2^length mineCount) -1
-- container idx means what cells it interact with
-- 00000 means it interact with cell 1,2,3,4,5
-- 00001 means it interact with cell 2,3,4,5
-- 10110 means it interact with cell 1,4
-- 10001 means it interact with cell 2,3,4
totalCase' :: [Int] -> [Int] -> [Int] -> Int
          -> Int
totalCase' mineCount [] [] currentCon = 1
totalCase' mineCount containers mineInCon currentCon
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

-- Given mine total on each cell and max container
-- of each container, how many possible cases there are?
totalCase :: [Int] -> [Int]
          -> Int
totalCase mineCount containers
    | currentSize > idealSize 
        = totalCase' mineCount
                     (take idealSize containers)
                     (replicate idealSize 0)
                     0
    | otherwise 
        = totalCase' mineCount
                     (containers ++ 
                      (replicate (idealSize-currentSize) 0))
                     (replicate idealSize 0)
                     0
    where idealSize = 2^(length mineCount) -1
          currentSize = length containers

-- Minus one for all related cells
minusMine :: [Int] -> Int
          -> [Int]
minusMine [] currentCon = []
minusMine (mine:mineCount) currentCon =
    if mod currentCon 2 == 0 then
        (mine-1) : minusMine mineCount (div currentCon 2)
    else
        mine : minusMine mineCount (div currentCon 2)

-- Modify nth element in a list with f
modifyNth :: [Int] -> (Int -> Int) -> Int
          -> [Int]
modifyNth [] f currentCon = []
modifyNth (container:containers) f 0 = (f container) 
                                       : containers
modifyNth (container:containers) f currentCon
    = container : modifyNth containers f (currentCon-1)

-- Cases if one container is guaranteed one mine
containerCase :: [Int] -> [Int] -> Int
              -> Int
containerCase mineCount containers currentCon
    = totalCase (minusMine mineCount currentCon)
                (modifyNth containers (\x -> x-1) currentCon)

-- Enumare containerCase for all containers
allConCase :: [Int] -> [Int]
           -> [Int]
allConCase mineCount containers
    | currentSize > idealSize 
        = allConCase mineCount
                     (take idealSize containers)
    | currentSize < idealSize
        = allConCase mineCount
                     (containers ++ 
                      (replicate (idealSize-currentSize) 0))
    | otherwise = map (containerCase mineCount containers)
                      index
    where idealSize = 2^(length mineCount) -1
          currentSize = length containers
          index = [0..((length containers) -1)]

main = do
    print (minimumMine 3 [8,2,10])
    print (addMines 3 [5,5,5,5] 2 0)
    print (countZero 2 0)
    print (minusMine [1,5,3,2,3] 8)
    print (totalCase [1] [3,0,0,0,0,0,0,0])
    print (allConCase [1] [3,0,0,0,0,0,0,0,0])
    print (totalCase [1,2] [2,1])
    print (allConCase [1,2] [2,1])
    print (totalCase [1,2,1] [1,1,0,3,1,0,3])
    print (allConCase [1,2,1] [1,1,0,3,1,0,3])
