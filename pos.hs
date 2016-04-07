import Data.Time.Calendar

taxRate :: Float
taxRate = 0.065

digitize :: Integer -> [Integer]
digitize 0 = []
digitize x = digitize (x `div` 10) ++ [x `mod` 10]

cardType :: Integer -> String
cardType x | head (digitize x) == 4 = "Visa"
		   | head (digitize x) == 5 = "MasterCard"
		   | head (digitize x) == 6 = "Discover"
		   | otherwise = "Not Accepted"

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate x y z



getDate :: IO (Integer, Int, Int)
getDate = getCurrentDate

return IO Bool