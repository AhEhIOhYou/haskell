module MainLab1  where

-- Функция max3, по трем целым возвращающая наибольшее из них.
max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max a (max b c)

-- Функция min3, по трем целым возвращающая наименьшее из них.
min3 :: Integer -> Integer -> Integer -> Integer
min3 a b c = min a (min b c)

-- Функция sort2, по двум целым возвращающая пару, в которой 
-- наименьшее из них стоит на первом месте, а наибольшее — на втором.
sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 a b = ((min a b), (max a b))

-- Функция bothTrue :: Bool -> Bool -> Bool, которая возвращает True тогда и только тогда, когда оба ее аргумента
-- будут равны True. Не используйте при определении функции стандартные логический операции (&&, || и т.п.).
bothTrue :: Bool -> Bool -> Bool
bothTrue a b = if a == True then
                if b == True then True
                    else False
                else False

-- Функция solve2::Double->Double->(Bool,Double),
-- которая по двум числам, представляющим собой коэффициенты
-- линейного уравнения ax + b = 0, возвращает пару, первый
-- элемент которой равен True, если решение существует и False
-- в противном случае; при этом второй элемент равен либо
-- значению корня, либо 0.0
solve2 :: Double -> Double -> (Bool, Double)
solve2 a b = if a == 0 then (False, 0.0) else
    (True, (-b)/ a)

-- Функция isParallel, возвращающая True, если два отрезка, концы которых задаются в аргументах функции, параллельны (или лежат на одной прямой). Например, значение выражения isParallel (1,1) (2,2) (2,0) (4,2) должно быть
-- равно True, поскольку отрезки (1, 1) − (2, 2) и (2, 0) − (4, 2) параллельны.
isParallel :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Bool
isParallel a b c d = if ((snd b - snd a) * (fst d - fst c) - (snd d - snd c) * (fst b - fst a)) == 0 then True
    else False

-- Функция isIncluded, аргументами которой служат параметры
-- двух окружностей на плоскости (координаты центров и радиусы);
-- функция возвращает True, если вторая окружность целиком содержится внутри первой.
isIncluded :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Integer -> Bool
isIncluded c1 c2 r1 r2 = if ((fst c2 - fst c1)^2 + (snd c2 - snd c1)^2) <= (r1 - r2)^2 then True
    else False


-- Функция isRectangular, принимающая в качестве параметров координаты трех точек на плоскости, и возвращающая True,
-- если образуемый ими треугольник — прямоугольный.
isRectangular :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Bool
isRectangular a b c = if ((fst b - fst a)^2 + (snd b - snd a)^2) == ((fst c - fst a)^2 + (snd c - snd a)^2) + ((fst c - fst b)^2 + (snd c - snd b)^2) then True
    else False

-- Функция isTriangle, определяющая, можно ли их отрезков с заданными длинами x, y и z построить треугольник
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle x y z = if (x + y > z) && (x + z > y) && (y + z > x) then True
    else False

-- Функция isSorted, принимающая на вход три числа и возвращающая True, если они упорядочены по возрастанию или по убыванию.
isSorted :: Integer -> Integer -> Integer -> Bool
isSorted a b c = if (a <= b) && (b <= c) then True
    else if (a >= b) && (b >= c) then True
    else False