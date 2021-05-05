doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber' x = (if x > 100
                      then x
                      else x*2) + 1
doubleSmallNumber x = if x > 100
                      then x
                      else x*2
boomBangs xs = [if x < 10 then "BOOM!" else "BAH!" | x <- xs, even x] 
lenght' xs = sum [1 | _ <- xs]
removeUppercase st = [c | c <- st, c `elem` ['а'..'я']]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a*a + b*b == c*c, a + b + c == 24]
difference a b = [x | x <- a, x `notElem` b]

hellodog y z q = [y * z + q | y <- [1..99], z <- [1..99], q <- [1..99]]

factorial :: Integer -> Integer
factorial n = product [1..n]
circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double 
circumference' r = 2 * pi * r

lucky 3 = "ORA,ORA,ORA!"
lucky x = "No lucky =("
factorial' 0 = 1
factorial' x = x * factorial' (x-1)
sumVector (x1, y1) (x2,y2) = (x1 + x2, y1 + y2)

head' [] = error "ТУ-ПИ-ЦА!"
head' (x:_) = x

firstLetter "" = "Упс, пустая строка!"
firstLetter all@(x:xs) = "Первая буква строки " ++ all ++ " это " ++ [x]

bmiTell weight height
 | bmi <= 18.5 = putStrLn "Слышь, эмо, ты дистрофик!"
 | bmi <= 25.0 = putStrLn "По части веса ты в норме. Зато, небось, уродец!"
 | bmi <= 30.0 = putStrLn "Ты толстый! Сбрось хоть немного веса!"
 | otherwise = putStrLn "Мои поздравления, ты жирный боров!"
 where bmi = weight / height ^ 2

solution number
 | number >= 0 = sum [x | x <- [1..number-1], x `div` 5 == 0 || x `div` 3 == 0]
 | number < 0 = 0

solution' n = sum $ [3,6..n-1] ++ [5,10..n-1]

highAndLow' :: [Char] -> [Char]
highAndLow' (x:xs) = x:' ': [last xs]
highAndLow' _ = ""

firstLetterb all@(x:xs) = "Первая буква строки " ++ all ++ " это " ++ [x]

highAndLow :: String -> String
highAndLow input = show (maxim $ map read $ words input :: [Int]) ++ " " ++ show (minim $ map read $ words input :: [Int])
 where
    minim [x]    = x
    minim (x:xs) = min x (minim xs)
    maxim [x]    = x
    maxim (x:xs) = max x (maxim xs)

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
    
standardDiscount = discount 1000 5

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt((fst p1 + fst p2)^2 + (snd p1 + snd p2)^2 )

doubleFact :: Integer -> Integer
doubleFact k
 | k <= 0 = 1
 | k > 0 = k * doubleFact (k - 2 ) 

doubleFact' :: (Num a, Enum a) => a -> a
doubleFact' k = product [k,k-2..1]


sPV xs ys = foldl (+) 0 (zipWith (*) xs ys)

fibonacci :: Integer -> Integer
fibonacci n
 | n >= 0 = helpMePLS (+) 1  0 n
 | otherwise =  helpMePLS (-) (-1) 0 (-n) 
 
helpMePLS :: (Eq t1, Num t1) => (t2 -> t2 -> t2) -> t2 -> t2 -> t1 -> t2
helpMePLS _ _ a 0 = a
helpMePLS o p a n = helpMePLS o a (p `o` a) (n-1)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n
 | n >= 0 = fibonacci' (n - 1) + fibonacci' (n - 2)
 | n < 0 = fibonacci' (n + 2) - fibonacci' (n + 1)



reverseLine xs = putStrLn $ reverse xs
