import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (asum)
import           Data.List            ((\\))
import qualified Data.List            as List

-- Runtime environment for GPS-like functions. Provides
-- a reader for available operations and a state for
-- the current conditions.
type GPS c a = ReaderT [Op c] (StateT [c] IO) a

runGPS :: [c] -> [Op c] -> GPS c a -> IO a
runGPS state ops prog =
  fst <$> runStateT (runReaderT prog ops) state

readOpsTable :: GPS c [Op c]
readOpsTable = ask

say :: MonadIO m => String -> m ()
say what = liftIO (putStrLn what)


-- GPS Version 1

gps1 :: Eq c => [c] -> [c] -> [Op c] -> IO Bool
gps1 state goals ops =
  runGPS state ops $
    (mapM_ achieve goals >> return True) <|> return False

achieve :: Eq c => c -> GPS c ()
achieve goal = do
  state <- get
  unless (goal `elem` state) $ do
     ops <- filter (wouldAchieve goal) <$> readOpsTable
     asum (map applyOp ops)

applyOp :: Eq c => Op c -> GPS c ()
applyOp op = do
  mapM_ achieve (preconds op)
  say ("Executing " ++ action op)
  modify (\state -> state `List.union` addList op \\ delList op)



-- Operation data type
data Op cond = Op
  { action   :: String
  , preconds :: [cond]
  , addList  :: [cond]
  , delList  :: [cond]
  } deriving Show

wouldAchieve :: Eq c => c -> Op c -> Bool
wouldAchieve cond op = cond `elem` addList op




data SchoolCond
  = CarNeedsBattery
  | CarWorks
  | HaveMoney
  | HavePhoneBook
  | InCommunicationWithShop
  | KnowPhoneNumber
  | ShopHasMoney
  | ShopKnowsProblem
  | SonAtHome
  | SonAtSchool
  deriving (Show, Eq)

schoolOps =
  [ Op { action = "drive-son-to-school"
       , preconds = [SonAtHome, CarWorks]
       , addList = [SonAtSchool]
       , delList = [SonAtHome]
       }
  , Op { action = "shop-installs-battery"
       , preconds = [CarNeedsBattery, ShopKnowsProblem, ShopHasMoney]
       , addList = [CarWorks]
       , delList = []
       }
  , Op { action = "tell-shop-problem"
       , preconds = [InCommunicationWithShop]
       , addList = [ShopKnowsProblem]
       , delList = []
       }
  , Op { action = "telephone-shop"
       , preconds = [KnowPhoneNumber]
       , addList = [InCommunicationWithShop]
       , delList = []
       }
  , Op { action = "loop-up-number"
       , preconds = [HavePhoneBook]
       , addList = [KnowPhoneNumber]
       , delList = []
       }
  , Op { action = "give-shop-money"
       , preconds = [HaveMoney]
       , addList = [ShopHasMoney]
       , delList = [HaveMoney]
       }
  ]

main :: IO ()
main = do
  say "Example 1:"
  print =<< gps1 [SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook] [SonAtSchool] schoolOps
  say "Example 2:"
  print =<< gps1 [SonAtHome, CarNeedsBattery, HaveMoney] [SonAtSchool] schoolOps
  say "Example 3:"
  print =<< gps1 [SonAtHome, CarWorks] [SonAtSchool] schoolOps
