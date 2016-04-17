import Data.Time.Clock
import Data.Time.Calendar
import Numeric

taxRate :: Float
taxRate = 0.065

digitize :: Integer -> [Integer]
digitize 0 = []
digitize x = digitize (x `div` 10) ++ [x `mod` 10]

cardType :: Integer -> String
cardType 0 = "Not Accepted"
cardType x | head (digitize x) == 4 = "Visa"
		   | head (digitize x) == 5 = "MasterCard"
		   | head (digitize x) == 6 = "Discover"
		   | otherwise = "Not Accepted"

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate y m d = do 
	currDate <- fmap utctDay getCurrentTime
	return((fromGregorian y m d) < currDate)

--Rounds input to 2 decimal places by using ShowFFloat (output is a string). 
--That string is then coverted back to a float using read :: Float.
roundTo :: Float -> Float
roundTo inputVal = read (showFFloat (Just 2) inputVal "") :: Float

calcTax :: Float -> Float
calcTax x = roundTo (x * taxRate)

verifyCard :: Integer -> Integer -> Int -> Int -> IO Bool
verifyCard cardNum y m d = do
	isPastDate <- pastDate y m d
	return (((cardType cardNum) /= "Not Accepted") && not isPastDate)

checkOut :: Float -> IO ()
checkOut num = do
	putStrLn "Enter Cost: "
	cost <- getLine
	let amount = (read cost :: Float)
	if amount /= 0
	  then do putStrLn (show amount)
		  checkOut (amount + num)
	else putStrLn (show (amount + (num + (calcTax (amount + num)))))



