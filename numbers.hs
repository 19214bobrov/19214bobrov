import Data.Char
import Data.List

charToInteger :: Char -> Int
charToInteger x | ord x >= 48 && ord x <= 57 = ord x - 48
                | ord x >= 65 && ord x <= 90 = ord x - 29
                | ord x >= 97 && ord x <= 122 = ord x - 87
                | otherwise = error "invalid char"
integerToChar :: Int -> Char
integerToChar x | x >= 0 && x <= 9 = chr (x + 48)
                | x >= 10 && x <= 35 = chr (x + 87)
                | x >= 36 && x <= 61 = chr (x + 29)
                | otherwise = error "invalid integer"
toDecimal :: Int -> String -> String
toDecimal base snumber | base < 1 || base > 61 = error "wrong input"
                       | otherwise = show $ f ((length snumber) - 1)(map charToInteger snumber) base
                        where
                             f n [] base = 0
                             f n (x:xs) base = x * base ^ n + f (n - 1) xs base
fromDecimal :: Int -> String -> String
fromDecimal toBase snumber | toBase < 1 || toBase > 61 = error "wrong input"
                           | otherwise = reverse (f (read snumber) toBase)
                            where
                                 f 0 toBase = []
                                 f number toBase = integerToChar ( number `mod` toBase) : (f (number `div` toBase) toBase)
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber