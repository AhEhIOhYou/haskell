module MainLab2 where

-- Определите функцию, принимающую на вход целое число n и возвращающую  список, содержащий n элементов, упорядоченных по возрастанию.

-- Список натуральных чисел.
funcNatural 0 = []
funcNatural n = funcNatural (n - 1) ++ [n]

-- Список нечетных натуральных чисел.
funcOdd 0 = []
funcOdd n = funcOdd (n - 1) ++ [2 * n - 1]

-- Список четных натуральных чисел.
funcEven 0 = []
funcEven n = funcEven (n - 1) ++ [2 * n]

-- Список квадратов натуральных чисел.
funcSquares 0 = []
funcSquares n = funcSquares (n - 1) ++ [n * n]

--  Список факториалов.
funcFactorials 0 = []
funcFactorials n = funcFactorials (n - 1) ++ [product [1..n]]

-- Список степеней двойки.
funcPowersOfTwo 0 = []
funcPowersOfTwo n = funcPowersOfTwo (n - 1) ++ [2 ^ n]

--  Список треугольных чисел n(n + 1)/2 - порождающая функция
funcTriangles 0 = []
funcTriangles n = funcTriangles (n - 1) ++ [n * (n + 1) `div` 2]

-- Список пирамидальных чисел n(n + 1)(2n + 1)/6 - порождающая функция
funcPyramids 0 = []
funcPyramids n = funcPyramids (n - 1) ++ [n * (n + 1) * (2 * n + 1) `div` 6]

-- Определите следующие функции:

-- Функция, принимающая на входе список вещественных чисел и
-- вычисляющую их арифметическое среднее. Постарайтесь, чтобы
-- функция осуществляла только один проход по списку.
funcAverage :: [Double] -> Double
funcAverage [] = 0
funcAverage nums = sum nums / fromIntegral (length nums)

-- Функция вычленения n-го элемента из заданного списка
funcNth :: [a] -> Integer -> a
funcNth [] _ = error "Out of bounds"
funcNth (x:xs) 0 = x
funcNth (x:xs) n = funcNth xs (n - 1)

-- Функция сложения элементов двух списков. Возвращает список,
-- составленный из сумм элементов списков-параметров. Учесть,
-- что переданные списки могут быть разной длины.
funcSum :: [Integer] -> [Integer] -> [Integer]
funcSum [] [] = []
funcSum [] (y:ys) = y : funcSum [] ys
funcSum (x:xs) [] = x : funcSum xs []
funcSum (x:xs) (y:ys) = (x + y) : funcSum xs ys

-- Функция перестановки местами соседних четных и нечетных
-- элементов в заданном списке
funcSwap :: [a] -> [a]
funcSwap [] = []
funcSwap [x] = [x]
funcSwap (x:y:xs) = y : x : funcSwap xs

-- Функция twopow n, которая вычисляет 2^n
-- если n четное, то n = 2 * k, 2^n = 2^(2*k) = (2^k)^2
-- если n нечетное, то = 2 * k + 1, 2^n = 2^(2*k + 1) = 2 * (2^k)^2
-- без использования ^
-- k - это n `div` 2
twopow :: Integer -> Integer
twopow 1 = 2
twopow n 
     | even n = twopow (n `div` 2) * twopow (n `div` 2)
     | odd n = 2 * (twopow ((n-1) `div` 2)) * (twopow ((n-1) `div` 2))

-- Функция removeOdd, которая удаляет из заданного
-- списка целых чисел все нечетные числа. Например:
-- removeOdd [1,4,5,6,10] должен возвращать [4,10].
removeOdd :: [Integer] -> [Integer]
removeOdd [] = []
removeOdd (x:xs) 
     | even x = x : removeOdd xs
     | odd x = removeOdd xs

-- Функция removeEmpty, которая удаляет пустые строки из заданного списка строк. Например:
-- removeEmpty ["", "Hello", "", "", "World!"]
-- возвращает ["Hello","World!"].
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) 
     | x == "" = removeEmpty xs
     | otherwise = x : removeEmpty xs

-- Функция countTrue :: [Bool] -> Integer, возвращающая количество элементов списка, равных True.
countTrue :: [Bool] -> Integer
countTrue [] = 0
countTrue (x:xs) 
     | x == True = 1 + countTrue xs
     | otherwise = countTrue xs

-- Функция makePositive, которая меняет знак
-- всех отрицательных элементов списка чисел, например: makePositive [-1, 0, 5, -10, -20] дает
-- [1,0,5,10,20]
makePositive :: [Integer] -> [Integer]
makePositive [] = []
makePositive (x:xs) 
     | x < 0 = (-x) : makePositive xs
     | otherwise = x : makePositive xs

-- Функция delete :: Char -> String -> String, которая принимает на вход строку и символ и возвращает
-- строку, в которой удалены все вхождения символа. Пример: delete ’l’ "Hello world!" должно возвращать "Heo
-- word!".
delete :: Char -> String -> String
delete _ [] = []
delete c (x:xs) 
     | c == x = delete c xs
     | otherwise = x : delete c xs

-- Функция substitute :: Char -> Char -> String -> String,
-- которая заменяет в строке указанный символ на заданный. Пример: substitute ’e’ ’i’ "eigenvalue" возвращает
-- "iiginvalui"
substitute :: Char -> Char -> String -> String
substitute _ _ [] = []
substitute search replace (x:xs) 
     | search == x = replace : substitute search replace xs
     | otherwise = x : substitute search replace xs
