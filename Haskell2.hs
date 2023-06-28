type Rel = String
type Name = String
type Value = String
type Type = String
type List_Concepts = [Concept]
type Concept = (Name, [Atrib])
type Atrib = (Name, Type)
type List_Indiv = [String]
type List_Relations = [String]
type List_Triplos = [Triplo]
type Triplo = (Name, Rel, Name, [At])
type At = (Name, Value)

ontoname :: String
ontoname = "map"

lc :: List_Concepts
lc = [("city", [("Name", "String")]),
      ("traveler", [("Name", "String")])]

li :: List_Indiv
li = ["pl", "ine", "pt", "lx"]

lr :: List_Relations
lr = ["alreadyvisited", "lives"]

lt :: List_Triplos
lt = [("pl", "iof", "traveler", [("name", "Paulo")]),
      ("ine", "iof", "traveler", [("name", "Ines")]),
      ("pt", "iof", "city", [("name", "Porto")]),
      ("lx", "iof", "city", [("name", "Lisboa")]),
      ("pl", "alreadyvisited", "pt", []),
      ("ine", "alreadyvisited", "lx", []),
      ("pl", "lives", "lx", []),
      ("ine", "lives", "pt", [])]

countOneOf :: List_Triplos -> Int
countOneOf [] = 0
countOneOf ((_, rel, _, _) : xs)
  | rel == "iof" = 1 + countOneOf xs
  | otherwise = countOneOf xs

countAllAndMoreOf :: List_Triplos -> Int
countAllAndMoreOf [] = 0
countAllAndMoreOf ((_, rel, _, _) : xs)
  | rel == "iof" || rel == "lives" = 1 + countAllAndMoreOf xs
  | otherwise = countAllAndMoreOf xs

generateDot :: List_Triplos -> String
generateDot [] = ""
generateDot ((src, rel, dst, _) : xs) =
  show src ++ "->" ++ show dst ++ " [arrowhead=dot, label=" ++ opLabel rel ++ "]" ++ "\n" ++ generateDot xs
  where
    opLabel "iof" = "properties"
    opLabel "lives" = "more-of"
    opLabel _ = "unknown"

main :: IO ()
main = do
  putStrLn "Count of OneOf: "
  print (countOneOf lt)
  putStrLn "Count of All and MoreOf: "
  print (countAllAndMoreOf lt)
  putStrLn "Dot code: "
  putStrLn (generateDot lt)