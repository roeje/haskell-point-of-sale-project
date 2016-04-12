import Data.Time.Clock
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

cDateTup = getCurrentTime >>= return.toGregorian.utctDay
--cDateTup2 <- fmap utctDay getCurrentTime
cDateTup2 = fmap utctDay getCurrentTime

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate x y z = do 
	dOne <- fmap utctDay getCurrentTime
	return((fromGregorian x y z) < dOne)

calcTax :: Float -> Float
calcTax x = 0.0

verifyCard :: Integer -> Integer -> Int -> Int -> IO Bool
verifyCard cNum y m d = do
	d1 = pastDate y m d
	return ((cardType cNum) /= "Not Accepted") && d1)

checkOut :: Float -> IO ()
checkOut num = do
	putStrLn "Enter Cost: "
	cost <- getLine
	let amount = (read cost :: Float) 
	if amount /= 0
		then do putStrLn (show amount)
				checkOut (amount + num)
	else putStrLn (show (amount + (num + (calcTax (amount + num)))))



