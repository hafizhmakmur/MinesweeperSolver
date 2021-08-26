import Combinatorics (sigma, phi, factorial, choose)

-- Helper functions
modifyNth :: Int -> Int -> [Int]
            -> [Int]
modifyNth val pos list
    | pos == 0 = val : (tail list)
    | otherwise = (head list) 
                  : (modifyNth val (pos-1) (tail list))

-- Main functions
totalPrevCell :: Int -> [Int]
              -> Int
totalPrevCell cellCount reducedCon = 0


totalNextCell :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int
             -> Int
totalNextCell currentCon
              cellCount 
              mineCount 
              container 
              reducedCon
              currentMineCount 
              = totalCase (currentCon+1)
                          cellCount
                          mineCount
                          container
                          (modifyNth currentMineCount 
                                     currentCon
                                     reducedCon)


-- currentCon = container that's currently examined
-- cellCount = how many cells there are
-- mineCount = mines in each containers
-- container = sizes of each containers (length = 2**cellCount-1)
-- reducedCon = reduced sizes of each containers
totalCase :: Int -> Int -> [Int] -> [Int] -> [Int] 
          -> Int
totalCase currentCon cellCount mineCount container reducedCon
    | (length reducedCon) == 0 
        = totalCase currentCon
                    cellCount
                    mineCount
                    container
                    (replicate (length container) 0)

    | currentCon < cellCount 
        = sigma 0 
                ((mineCount!!currentCon) 
                    - (totalPrevCell currentCon reducedCon))
                (totalNextCell currentCon
                               cellCount
                               mineCount
                               container
                               reducedCon)

    | otherwise = product $ zipWith choose container reducedCon

main = do
    print $ factorial 5
