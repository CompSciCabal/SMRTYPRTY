import Text.Pretty.Simple (pPrint)
import qualified Data.List as List
import Data.List ((\\))

-- Operation data type
data Op cond = Op
  { action   :: OpName
  , preconds :: [cond]
  , addList  :: [cond]
  , delList  :: [cond]
  } deriving (Show, Eq)

type OpName = String

wouldAchieve :: Eq c => c -> Op c -> Bool
wouldAchieve cond op = cond `elem` addList op

precondsApply :: Eq c => [c] -> Op c -> Bool
precondsApply state op = all (\c -> c `elem` state) (preconds op)

gps2 :: Eq c => [c] -> [c] -> [Op c] -> [([OpName], [c])]
gps2 state goals ops =
  filter (satisfied goals) (generate state ops)
    where
      satisfied goals (_, st) = all (\g -> g `elem` st) goals

generate :: Eq c => [c] -> [Op c] -> [([OpName], [c])]
generate state ops =
  let ns = nextSteps state ops
      as = map (\(newState, op, _) -> ([op], newState)) ns
      bs = concatMap (\(newState, op, newOps) -> map (\(xs,st) -> (op:xs, st)) (generate newState newOps)) ns
   in as ++ bs

nextSteps :: Eq c => [c] -> [Op c] -> [([c], OpName, [Op c])]
nextSteps state ops =
  [ (applyOp state op, action op, List.delete op ops) | op <- ops, precondsApply state op ]

applyOp :: Eq c => [c] -> Op c -> [c]
applyOp state op =
  (state \\ delList op) ++ addList op


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
  , Op { action = "look-up-number"
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

data BananaCond
  = ChairAtMiddleRoom
  | AtMiddleRoom
  | OnFloor
  | AtBananas
  | OnChair
  | ChairAtDoor
  | AtDoor
  | EmptyHanded
  | HasBall
  | HasBananas
  | NotHungry
  | Hungry
  deriving (Show, Eq)

bananaOps =
  [ Op { action = "climb-on-chair"
       , preconds = [ChairAtMiddleRoom, AtMiddleRoom, OnFloor]
       , addList = [AtBananas, OnChair]
       , delList = [AtMiddleRoom, OnFloor] -- EmptyHanded?
       }
  , Op { action = "push-chair-to-middle"
       , preconds = [ChairAtDoor, AtDoor]
       , addList = [ChairAtMiddleRoom, AtMiddleRoom]
       , delList = [ChairAtDoor, AtDoor]
       }
  , Op { action = "walk-to-middle"
       , preconds = [AtDoor, OnFloor] -- WalkTheDinosaur?
       , addList = [AtMiddleRoom]
       , delList = [AtDoor]
       }
  , Op { action = "grasp-bananas"
       , preconds = [AtBananas, EmptyHanded]
       , addList = [HasBananas]
       , delList = [AtDoor]
       }
  , Op { action = "drop-ball"
       , preconds = [HasBall]
       , addList = [EmptyHanded]
       , delList = [HasBall]
       }
  , Op { action = "eat-bananas"
       , preconds = [HasBananas]
       , addList = [EmptyHanded, NotHungry]
       , delList = [HasBananas, Hungry]
       }
  ]

data MazeCond a = At a deriving (Show, Eq)

exampleMaze :: [(Int, Int)]
exampleMaze =
  [ (1, 2), (2, 3), (3, 4), (4, 9)
  , (9, 14), (9, 8), (8, 7), (7, 12)
  , (12, 13), (12, 11), (11, 6), (11, 16)
  , (16, 17), (17, 22), (21, 22), (22, 23)
  , (23, 18), (23, 24), (24, 19), (19, 20)
  , (20, 15), (15, 10), (10, 5), (20, 25)
  ]
makeMaze = concatMap makeMazeOps
makeMazeOps (a, b) = [ makeMazeOp a b, makeMazeOp b a ]
makeMazeOp a b =
  Op { action = "move from " ++ show a ++ " to " ++ show b
     , preconds = [At a]
     , addList = [At b]
     , delList = [At a]
     }



main :: IO ()
main = do
  putStrLn "** Getting the kid to school"
  putStrLn "Example 1:"
  pPrint $ gps2 [SonAtHome, CarNeedsBattery, HaveMoney, HavePhoneBook] [SonAtSchool] schoolOps
  putStrLn "Example 2:"
  pPrint $ gps2 [SonAtHome, CarNeedsBattery, HaveMoney] [SonAtSchool] schoolOps
  putStrLn "Example 3:"
  pPrint $ gps2 [SonAtHome, CarWorks] [SonAtSchool] schoolOps

  putStrLn "** Monkey and Bananas"
  pPrint $ gps2 [AtDoor, OnFloor, HasBall, Hungry, ChairAtDoor] [NotHungry] bananaOps

  putStrLn "** Maze Solving"
  pPrint $ gps2 [At 1] [At 25] (makeMaze exampleMaze)
