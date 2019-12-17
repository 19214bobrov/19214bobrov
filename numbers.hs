import Data.Char
import Data.List

charToInteger :: Char -> Integer
charToInteger x | (ord x) >= ord 'a' && (ord x) <= ord 'z' = toInteger $ ord x - ord 'a' + 10
                | (ord x) >= ord '0' && (ord x) <= ord '9' = toInteger $ ord x - ord '0'
                | (ord x) >= ord 'A' && (ord x) <= ord 'Z' = toInteger $ ord x - ord 'A' + 36
                | otherwise = error "invalid char"
integerToChar :: Integer -> Char
integerToChar x | x >= 0 && x <= 9 = chr (fromInteger x + ord '0')
                | x >= 10 && x <= 35 = chr (fromInteger x + ord 'a' - 10)
                | x >= 36 && x <= 61 = chr (fromInteger x + ord 'A' - 36)
                | otherwise = error "invalid integer"
toDecimal :: Integer -> String -> String
toDecimal 1 snumber | all (=='1') snumber = show $ length snumber
                    | otherwise = error "invalid character"
toDecimal base snumber | base >= 1 && base <= 61 = show (answer acc $ length acc)
                       | otherwise = error "base invalid"
                        where
                             acc = map charToInteger snumber
                             answer [] _ = 0
                             answer (a:acc) power | a < base = a * (base ^ (power - 1)) + answer acc (power - 1)
fromDecimal :: Integer -> String -> String
fromDecimal 1 snumber | any(\char -> ord char < 48 || ord char > 57) snumber = error "incorrect number"
                      | otherwise = replicate (read snumber) '1'
fromDecimal toBase snumber | any(\char -> ord char < 48 || ord char > 57) snumber = error "incorrect number"
                           | toBase <= 61 && toBase > 1 = map integerToChar (answer number [])
                           | otherwise = error "invalid base"
                            where
                                 number = read snumber
                                 answer 0 acc = acc
                                 answer number acc = (answer ( number `div` toBase) (number `mod` toBase : acc))
convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber
