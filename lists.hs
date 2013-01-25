import Data.List

--1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

--2
myAlmostLast :: [a] -> a
myAlmostLast [x,_] = x
myAlmostLast (_:xs) = myAlmostLast xs

--3
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i-1)

--4
myLength :: [a] -> Int
myLength = foldl (\n _ -> n+1) 0 

--5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--6
palindrome :: (Eq a) => [a] -> Bool
palindrome list = list == (myReverse list)

--7
flatten [] = []
--flatten (xs) = foldr ++ [] $ map (flatten xs)

--8
compress :: Eq a => [a] -> [a]
compress = map head . group

--9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

--10
lengthEncode :: Eq a => [a] -> [(Int,a)]
lengthEncode [] = []
lengthEncode x = zip (map length (pack x)) (map head (pack x))  

--another 10 option that's really sexy (copied)
encode xs = (enc . pack) xs
  where enc = foldr (\x acc -> (length x, head x) : acc) []


  
