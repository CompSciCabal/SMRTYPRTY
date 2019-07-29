{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Arrow
import           Control.Monad        (forever)
import           Data.Char
import           Data.Functor
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Maybe           as Maybe
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Void            (Void)
import           Prelude              hiding (Word)
import           System.IO
import           Test.QuickCheck      hiding (label)
import           Text.Megaparsec
import           Text.Megaparsec.Char


-- Our rule set is a list of patterns matched with
-- their possible responses.
type RuleSet = [Rule]
type Rule = (Pattern, [Response])

-- A pattern is a list of items that match either
-- a literal word or a named pattern.
type Pattern = [Matchable]
data Matchable = Lit Word | Var Name deriving Show

-- A response is a list of printable items - either
-- a literal string or a variable reference.  Technically
-- the item data type is different from Pattern, but
-- I'm reusing it here for convenience.
type Response = [Printable]
type Printable = Matchable

type Name = Text
type Env a = Map Name a


-- Normally, main is declared as IO (), but we say
-- "IO a" here to show that it's not going to return.
main :: IO a
main = do
  rules <- loadRules "rules.lisp"
  print rules
  putStrLn "I am Eliza. Tell me your problems."
  elizaLoop (elizaRespond rules)

-- Read a line, and print Eliza's response. Repeat forever.
elizaLoop :: ([Word] -> Gen Text) -> IO a
elizaLoop eliza = forever $ do
  putStr ">>> "
  hFlush stdout
  line <- splitWords <$> T.getLine
  response <- generate (eliza line)
  T.putStrLn response

-- Given the rule set and a line of input, find
-- the first rule that matches, and return one
-- of its responses.
elizaRespond :: RuleSet -> [Word] -> Gen Text
elizaRespond rules input =
  case elizaMatch rules input of
    [] -> pure "Say something!"
    ((_, resps), e) : _ ->
      randomResponse (map (unpackWords . elizaSubst e) resps)

-- Find the first rule that matches the input
elizaMatch :: RuleSet -> [Word] -> [(Rule, Env [Word])]
elizaMatch ruleset input =
  Maybe.mapMaybe (matchRule input) ruleset

-- Does this rule match the input?  If so, return the rule
-- and the variables that matched.
matchRule :: [Word] -> Rule -> Maybe (Rule, Env [Word])
matchRule input rule@(pattern,_) =
  case matchInput input pattern of
    Nothing  -> Nothing
    Just env -> Just (rule, env)

-- The core matcher.
matchInput :: [Word] -> Pattern -> Maybe (Env [Word])
matchInput [] [] = Just Map.empty
matchInput _ [] = Nothing
matchInput [] _ = Nothing
matchInput (t:ts) (Lit x:xs)
  | t == x = matchInput ts xs
  | otherwise = Nothing
matchInput (t:ts) (Var x:xs)
 = choice
     [ (matchInput (t:ts) xs <&> Map.insert x []) -- empty match
     , (matchInput ts (Var x : xs) <&> Map.adjust (t:) x) -- continue match
     , (matchInput ts xs <&> Map.insert x [t]) -- single word match
     ]

-- Substitute variables into the response, switching
-- personal viewpoint (me <-> you) as necessary.
elizaSubst :: Env [Word] -> Response -> [Word]
elizaSubst env =
  map sub
    where
      sub (Lit w) = w
      sub (Var v) =
        case Map.lookup v env of
          Nothing  -> W ("?" <> v)
          Just val -> joinWords (switchViewpoint val)

-- Switch personal viewpoint, word by word
switchViewpoint :: [Word] -> [Word]
switchViewpoint = map (sub viewpointWords)
  where
    sub m w = case Map.lookup w m of
                Nothing -> w
                Just w' -> w'

viewpointWords :: Map Word Word
viewpointWords = bidirectionalIndex
  [ ("me", "you")
  , ("I", "you")
  , ("am", "are")
  , ("my", "your")
  , ("I'm", "you're")
  ]

bidirectionalIndex :: Ord a => [(a, a)] -> Map a a
bidirectionalIndex d = Map.fromList (d ++ map (\(x, y) -> (y, x)) d)


-- convenience function to pick a random response.
randomResponse :: [a] -> Gen a
randomResponse choices = oneof (map pure choices)




-- RULES PARSER

type Parser = Parsec Void Text

-- Load rules from a Lisp file.
loadRules :: FilePath -> IO RuleSet
loadRules path = do
  input <- T.readFile path
  (fail.errorBundlePretty ||| return) $ parse ruleset path input

-- Parser rules always assume that white space has been consumed,
-- and they are responsible for consuming trailing space when legal
-- to do so.
ruleset :: Parser RuleSet
ruleset = parens (tok "defparameter" >> tok "*eliza-rules*" >> quoteChar >> lispList ruleDef)
  where
    ruleDef = parens $ do
      pat <- pattern
      responses <- response `sepBy` space
      return (pat, map compress responses)

pattern :: Parser Pattern
pattern = label "pattern" (lispList matchable)
  where
    matchable = choice [splat, lit]
    lit = Lit . W <$> atom
    splat = Var <$> parens (tok "?*" >> splatName)
    splatName = char '?' >> atom

response :: Parser Response
response = label "response" (lispList printable)
  where
    printable = choice [try ref, lit]
    lit = Lit . W <$> atom
    ref = Var <$> (char '?' >> atom)

quoteChar :: Parser ()
quoteChar = void (char '\'')

parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')' <* space

lispList :: Parser a -> Parser [a]
lispList p = parens (p `sepBy` space)

tok :: Text -> Parser ()
tok t = string t >> space >> return ()

atom :: Parser Text
atom = space >> takeUntil (\ch -> ch == '(' || ch == ')' || isSpace ch)
  where takeUntil p = takeWhile1P Nothing (not . p)

-- Optimize a response by joining consecutive literals.
-- For example,
--
-- compress [Lit "Have", Lit "you", Lit "dreamt", Var "x", Lit "before?"]
--       == [Lit "Have you dreamt", Var "x", Lit "before?"]
compress :: [Printable] -> [Printable]
compress [] = []
compress (Var x : rest) = Var x : compress rest
compress (Lit x : rest) =
  case compress rest of
    Lit y : ys -> Lit (joinWords [x, y]) : ys
    ys         -> Lit x : ys


-- WORD DATATYPE

-- A Word is a piece of text that we are going to compare
-- case insensitively.
newtype Word = W { unW :: Text } deriving IsString

instance Ord Word where
  compare (W a) (W b) = compare (T.toCaseFold a) (T.toCaseFold b)

instance Eq Word where
  W a == W b = T.toCaseFold a == T.toCaseFold b

instance Show Word where
  show (W a) = show a

-- Split a raw line of input into words
splitWords :: Text -> [Word]
splitWords = map W . T.words

joinWords :: [Word] -> Word
joinWords = W . T.unwords . map unW

unpackWords :: [Word] -> Text
unpackWords = T.unwords . map unW
