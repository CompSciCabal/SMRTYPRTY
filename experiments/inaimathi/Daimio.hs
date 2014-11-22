module Daimio where

data Thing = Str String
           | Int Integer
           | Flt Float
           | Seq [(Thing, Thing)] deriving (Eq, Ord, Show, Read)

data Space = Space String (Thing -> Maybe Thing)

data Path = Branch Space [Path]
          | Leaf (Thing -> IO ())

data Box = Box (IO Thing) Path

runBox :: Box -> IO ()
runBox (Box input path) = do msg <- input
                             runPath msg path

runPath :: Thing -> Path -> IO ()
runPath msg (Leaf io) = do { _ <- io msg ; unit }
runPath msg (Branch (Space _ fn) paths) = when (fn msg) recur
        where recur v = mapM_ (runPath v) paths

---------- Utility
unit :: IO ()
unit = return ()

when :: Maybe a -> (a -> IO()) -> IO ()
when m io = maybe unit io m

---------- The hello world setup
hello :: Space
hello = Space "Greeter" greets
    where greets (Str name) = Just . Str $ concat ["Hello, ", name, "!"]
          greets _          = Just . Str $ "Umm. I have to go now. Bye."

helloBox :: Box
helloBox = Box getLn (Branch hello [Leaf putLn])
    where getLn = do putStrLn "What's your name?"
                     ln <- getLine
                     return $ Str ln
          putLn (Str msg) = putStrLn msg
          putLn t         = putStrLn $ show t

main :: IO ()
main = do _ <- runBox helloBox
          unit
