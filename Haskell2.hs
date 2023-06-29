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
lr = ["alreadyVisited", "lives"]

lt :: List_Triplos
lt = [("pl", "iof", "traveler", [("name", "Paulo")]),
      ("ine", "iof", "traveler", [("name", "Ines")]),
      ("pt", "iof", "city", [("name", "Porto")]),
      ("lx", "iof", "city", [("name", "Lisboa")]),
      ("pl", "alreadyVisited", "pt", []),
      ("ine", "alreadyVisited", "lx", []),
      ("pl", "lives", "lx", []),
      ("ine", "lives", "pt", [])]

countOneOf :: List_Triplos -> Int
countOneOf = length . filter (\(_, rel, _, _) -> rel == "iof")

countAllAndMoreOf :: List_Triplos -> Int
countAllAndMoreOf = length . filter (\(_, rel, _, _) -> rel == "iof" || rel == "lives")

generateDot :: List_Triplos -> String
generateDot lt =
  "digraph " ++ ontoname ++ " {\n" ++
  generateNodes lc ++
  generateEdges lt ++
  "}"
  where
    generateNodes [] = ""
    generateNodes ((name, _) : xs) =
      "\"" ++ name ++ "\" [shape=ellipse, style=filled, color=turquoise4];\n" ++
      "\"" ++ name ++ "\" [shape=rectangle, color=turquoise4];\n" ++
      generateNodes xs
    
    generateEdges [] = ""
    generateEdges ((src, rel, dst, _) : xs) =
      showNode src ++ "->" ++ showNode dst ++ " [label=" ++ show rel ++ ", style=dotted, color=red];\n" ++
      showNode src ++ "->" ++ showNode ("name='" ++ dst ++ "'") ++ " [label=\"properties\", style=dotted, color=red];\n" ++
      generateEdges xs
      where
        showNode name = "\"" ++ name ++ "\""

main :: IO ()
main = do
  putStrLn "Count of OneOf:"
  print (countOneOf lt)
  putStrLn "Count of All and MoreOf:"
  print (countAllAndMoreOf lt)
  putStrLn "Dot code:"
  putStrLn (generateDot lt)