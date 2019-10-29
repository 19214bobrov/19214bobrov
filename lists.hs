--1
get' :: [a] -> Integer -> a
get' [] n = error "no such element in the list"
get' (x:xs) 0 = x
get' (x:xs) n = get' xs (n - 1)


--2
head' :: [a] -> a
head' [] = error "empty list"
head' (x:xs) = x


--3
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (x:xs) = last' xs


--4
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (x:xs) = xs


--5
init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x : init' xs


--6
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = append' (reverse xs) x



--7
length' :: [a] -> Integer
length' xs = helper 0 xs where
                         helper n [] = n
                         helper n (x:xs) = helper (n + 1) xs


--8
append' :: [a] -> a -> [a]
append' [] k = [k]
append' (x:xs) k = x : append' xs k


--9?
concat' :: [a] -> [a] -> [a]
concat' x [] = x
concat' xs (y:ys) = concat' (append' xs y) ys


--10 
drop' :: Integer -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n - 1) xs


--11
take' :: Integer -> [a] -> [a]
take' 0 xs = []
take' n (x:xs) = x : take' (n - 1) xs


--12
splitAt' :: Integer -> [a] -> ([a],[a])
splitAt' n xs = (take' (n - 1) xs, drop' (n - 1) xs)


--13
null' :: [a] -> Bool
null' [] = True
null' xs = False


--14
elem' :: Eq a => [a] -> a -> Bool
elem' [] k = False
elem' (x:xs) k = if k == x then True else elem' xs k


--15
filter' :: (a -> Bool) -> [a] -> [a]
filter' test [] = []
filter' test (x:xs) = if test x then x : filter' test xs else filter' test xs


--16
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs




--17
zip' :: [a] -> [b] -> [(a,b)]
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
 
