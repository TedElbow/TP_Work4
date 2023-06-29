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
countOneOf = length . filter (\(_, rel, _, _) -> rel == "iof")

countAllAndMoreOf :: List_Triplos -> Int
countAllAndMoreOf = length . filter (\(_, rel, _, _) -> rel == "iof" || rel == "lives")

generateDot :: List_Triplos -> String
generateDot = unlines . (["digraph " ++ ontoname ++ " {"] ++) . (++ ["}"]) . map generateEdge
  where
    generateEdge (src, rel, dst, _) =
      showNode src ++ " [shape=rectangle, style=filled, color=goldenrod];" ++
      showNode dst ++ " [shape=rectangle, style=filled, color=goldenrod];" ++
      show src ++ "->" ++ show dst ++ " [label=" ++ show rel ++ ", style=" ++ opStyle rel ++ "];"
    showNode name = "\"" ++ name ++ "\""
    opStyle "iof" = "dashed"
    opStyle "lives" = "dashed"
    opStyle _ = "dotted"

main :: IO ()
main = do
  putStrLn "Count of OneOf:"
  print (countOneOf lt)
  putStrLn "Count of All and MoreOf:"
  print (countAllAndMoreOf lt)
  putStrLn "Dot code:"
  putStrLn (generateDot lt)