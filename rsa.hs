import Data.Char

rsa :: (Integer, Integer) -> String -> [Integer]
rsa (e, n) = map $ (\p -> p^e `mod` n) . (\c -> toInteger (ord (toUpper c) - ord 'A'))

findPrimes :: Integer -> (Integer, Integer)
findPrimes n = (p, q)
    where p = head [x | x <- [3,5..], n `mod` x == 0]
          q = div n p

gcde :: Integer -> Integer -> (Integer, Integer)
gcde n m | n `mod` m == 0 = (0, 1)
         | otherwise = (y, x - y * (div n m))
         where (x, y) = gcde m (n `mod` m)

modInv :: Integer -> Integer -> Integer
modInv n m | i < 0 = i + m
           | otherwise = i
           where i = fst $ gcde n m

phi :: Integer -> Integer
phi n = toInteger $ length [x | x <- [1..n], gcd n x == 1]

secretKey :: (Integer, Integer) -> Integer
secretKey (e, n) = modInv e (phi n)

decryptRsa :: (Integer, Integer) -> [Integer] -> [Integer]
decryptRsa (e, n) = map $ (\p -> p^e `mod` n)

crackRsa :: (Integer, Integer) -> [Integer] -> [Integer]
crackRsa (e, n) = decryptRsa (modInv e (phi n), n)

split2 :: [Integer] -> [Integer]
split2 [] = []
split2 (x:xs) = div x 100 : x `mod` 100 : split2 xs

digitToChar :: Integer -> Char
digitToChar n = chr (ord 'A' + fromInteger n)

ex1 :: (Integer, Integer) -> [Integer] -> String
ex1 key xs = map digitToChar (split2 (crackRsa key xs))

