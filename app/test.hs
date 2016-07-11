import System.IO
import Control.Monad
import Data.Maybe
import Data.List
import Debug.Trace

data State = State
    { speed :: Int
    , posx :: Int
    , gap :: !Int
    , road :: !Int
    , platform :: !Int
    }
    deriving Show

validState :: State -> Bool
validState state =
    speed > gap + (traceShow (rem (road - posx + 1) speed, road-posx, speed) (rem (road - posx + 1) speed))
    where (State speed posx gap road platform) = state

--Invalid states return false
statefilter :: State -> Bool
statefilter state
  | speed == 0 && posx < road + gap = False
  | posx >= road + gap + platform = False
  | posx >= road && posx < road + gap = False
  | otherwise = True
  where (State speed posx gap road platform) = state

target :: State -> Bool
target state =
  let minpos = road + gap
      maxpos = road + gap + platform - 1
      (State speed posx gap road platform) = state
  in
  speed == 0 && posx >= minpos && posx <= maxpos

type Plan = [Action]

search :: [(Plan, State)] -> Plan
search options
    | isNothing result = search $ filter (\(p,s) -> statefilter s) expanded
    | isJust result = fst (fromJust result)
    where
        result = find (\(_, s) -> target s) options :: Maybe (Plan, State)
        expanded = concatMap expand options :: [(Plan, State)]


expand :: (Plan, State) -> [(Plan, State)]
expand (path, state) = map (\a -> (a : path, expandAction state a)) ["SPEED", "JUMP", "SLOW", "WAIT", "SPEED"]

expandAction :: State -> Action -> State
expandAction state action
    | action == "WAIT" = state {posx = posx + speed}
    | action == "SLOW" = state {posx = posx + speed - 1, speed = speed - 1}
    | action == "JUMP" = state {posx = posx + speed}
    | action == "SPEED" = state {posx = posx + speed +1, speed = speed + 1}
    where (State speed posx gap road platform) = state




type Action = String




--Take a list of states and try to reach the goal state from there.
--planSearch :: [([Action], State)]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let road = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let gap = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let platform = read input_line :: Int -- the length of the landing platform.
    let state = State { road = road, gap = gap, platform = platform, speed = 0, posx = 0 }
    loop state

loop :: State -> IO ()
loop stateOld = do
    input_line <- getLine
    let speed_ = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let coordx = read input_line :: Int -- the position on the road of the motorbike.

    let state@(State speed posx gap road platform) = stateOld {speed = speed_, posx = coordx}
    -- hPutStrLn stderr "Debug messages..."
    hPutStrLn stderr $ show state
    let plan = search [([], state)] :: Plan
    hPrint stderr plan

    -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.

    let action = if coordx >= road + gap then "SLOW" else last plan
    putStrLn action

    loop state
