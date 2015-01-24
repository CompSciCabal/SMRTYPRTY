module FBP where

data Thing = Str String
           | Num Integer
           | Flt Float
           | Lst [Thing]
           | Seq [(Thing, Thing)] deriving (Eq, Ord, Show, Read)

data Message = Msg String Thing deriving (Eq, Ord, Show, Read)

data Path = Branch (Thing -> Maybe Thing) [Path]
          | Leaf (Thing -> Message)

data Part = Route Path
          | Crate Box

data Box = Box [(String, Part)]

runPart :: Message -> Part -> [Message]
runPart msg@(Msg tag _) (Crate (Box tbl)) = 
    concatMap (runPart msg) . map snd $ filter ((==tag) . fst) tbl
runPart (Msg _ thing) (Route path) = runPath thing path

runPath :: Thing -> Path -> [Message]
runPath thing (Branch fn ps) = case fn thing of
                                 Just v -> concatMap (runPath v) ps
                                 Nothing -> []
runPath thing (Leaf fn) = [fn thing]

---------- Basics
hello :: Path
hello = Leaf (out . body)
    where out = Msg "out" . Str
          body (Str name) = "Hello there, " ++ name
          body _          = "Umm. Ok?" 

greeter :: Part
greeter = Crate $ Box [("in", Route hello)]

printer :: Message -> IO ()
printer (Msg _ t) = printThing t
    where printThing (Str s) = putStrLn s
          printThing t = putStrLn $ show t

main :: IO ()
main = mapM_ printer $ runPart (Msg "in" (Str "Leo")) greeter 
