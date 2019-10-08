mapleft :: (a -> b) -> [a] -> [b]
mapleft fun xs = foldl (\batt x -> batt ++ [fun x]) [] xs


mapright :: (a -> b) -> [a] -> [b]
mapright fun xs = foldr (\x batt -> fun x: batt) [] xs