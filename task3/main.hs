module MainLab3 where

-- База данных идентификаторов программы представляет собой список пар
-- состоящих из имени идентификатора и его типа.
type Database = [(String, Type)]

-- Тип данных для представления типов идентификаторов программы.
data Type = IntType | BoolType | StringType | ListType Type | TupleType [Type]
  deriving (Eq, Show)

-- 1) isStructured, проверяет, что аргумент является сложным типом.
isStructured :: Type -> Bool
isStructured (ListType _) = True
isStructured (TupleType _) = True
isStructured _ = False

-- 2) GetType, по заданному имени и списку идентификаторов возвращает тип идентификатора,
-- с указанным именем. Если идентификатора с указанным именем нет в списке, то возвращается error
getType :: String -> Database -> Type
getType _ [] = error "No such identifier"
getType name ((id, t):xs) = if name == id then t else getType name xs

-- 3) getFields, по заданному имени возвращает список полей идентификатора, если он имеет тип структуры.
-- Если идентификатор не имеет тип структуры, то возвращается пустой список.
getFields :: String -> Database -> [Type]
getFields name db = case getType name db of
  ListType t -> [t]
  TupleType ts -> ts
  _ -> []

-- 4) getByType, по заданному типу возвращает список идентификаторов, имеющих данный тип.
getByType :: Type -> Database -> [String]
getByType t db = [name | (name, tp) <- db, tp == t]

-- 5) getByTypes, по заданному списку типов возвращает список идентификаторов, имеющих один из указанных типов.
getByTypes :: [Type] -> Database -> [String]
getByTypes ts db = [name | (name, tp) <- db, tp `elem` ts]

db :: Database
db = [("x", IntType), ("y", BoolType), ("z", ListType IntType), ("t", TupleType [IntType, BoolType])]
test1 = isStructured (ListType IntType)
test2 = getType "x" db
test3 = getType "y" db
test4 = getType "z" db
test5 = getType "t" db
test6 = getFields "x" db
test7 = getFields "y" db

test8 = getFields "z" db
test9 = getFields "t" db
test10 = getByType IntType db
test11 = getByType BoolType db
test12 = getByType (ListType IntType) db
test13 = getByType (TupleType [IntType, BoolType]) db
test14 = getByTypes [IntType, BoolType] db
test15 = getByTypes [IntType, BoolType, StringType] db

main :: IO ()
main = do
  print test1
  print test2
  print test3
  print test4
  print test5
  print test6
  print test7
  print test8
  print test9
  print test10
  print test11
  print test12
  print test13
  print test14
  print test15

-- Output:
-- True
-- IntType
-- BoolType
-- ListType IntType
-- TupleType [IntType,BoolType]
-- []
-- []
-- [IntType]
-- [IntType,BoolType]
-- ["x","z"]
-- ["y"]
-- ["z"]
-- ["t"]
-- ["x","y","z","t"]
-- ["x","y","z","t"]
