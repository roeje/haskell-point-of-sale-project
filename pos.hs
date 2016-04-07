taxRate :: Foat
let taxRate = 0.065

digitize :: Integer -> [Integer]
digitize x = digitize (x 'div' 10) ++ [x 'mod' 10]

