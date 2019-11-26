data Complex a = Complex a a                

instance (Show a) => Show (Complex a) where
   show (Complex real image) = show real ++ "+" ++ show image ++ "i"

instance (Eq a) => Eq (Complex a) where
   (Complex real0 image0) == (Complex real1 image1) = (real0 == real1) && (image0 == image1)


data QuantumState a = QuantumState a String

instance (Show a) => Show (QuantumState a) where
   show (QuantumState complex state) = show complex ++ " " ++ state

instance (Eq a) => Eq (QuantumState a) where
   (QuantumState complex0 state0) == (QuantumState complex1 state1) = (complex0 == complex1) && (state0 == state1)

instance Functor QuantumState where
   fmap f (QuantumState complex state) = QuantumState (f complex) state 


type Qubit a = [QuantumState a]

toList :: Qubit (Complex a) -> [Complex a]
toList [] = []
toList ((QuantumState complex state):xs) = complex:(toList xs)

toLabelList :: Qubit (Complex a) -> [String]
toLabelList [] = []
toLabelList ((QuantumState complex state):xs) = state:(toLabelList xs)

fromList :: [Complex a] -> [String] -> Qubit (Complex a)
fromList [] [] = []
fromList (x:xs) (y:ys) = (QuantumState x y): fromList xs ys

toPairList :: Qubit (Complex a) -> [(Complex a, String)]
toPairList [] = []
toPairList ((QuantumState complex state):xs) = (complex, state):(toPairList xs)

fromPairList :: [(Complex a, String)] -> Qubit (Complex a)
fromPairList [] = []
fromPairList ((complex, state):xs) = (QuantumState complex state):fromPairList xs

--scalarProduct :: (Num a) => Qubit (Complex a) -> Qubit (complex a) -> Complex a
--будет доделано(разбираюсь) ♥♥♥

--entagle :: (Num a) => Qubit (Complex a) -> Qubit (Complex a) -> Qubit (Complex a)
--будет доделано(разбираюсь) ♥♥♥

