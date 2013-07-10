import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Random
import Debug.Trace
import Control.Applicative
import Data.Maybe

-- make things a bit more readable
type Name = String
type Count = Int
type Rate = Float
type Propensity = Float
type PropensitySum = Float
type Time = Float
type Input = Chemical
type Output = Chemical

data Chemical = Chemical Name deriving (Show, Ord, Eq)
data Reaction = ZeroOrd Rate Output | 
                    FstOrd Rate Input Output |
                    SndOrd Rate Input Input Output | 
                    Assoc Rate Input Output | 
                    Disassoc Rate Input Output deriving (Show)

data System = System (M.Map Chemical Count) deriving (Show)

inputCount i m = M.findWithDefault 0 i m

calcPropensity :: System -> Reaction -> Propensity
calcPropensity _ (ZeroOrd r _) = r
calcPropensity (System m) (FstOrd r i _) = r * (fromIntegral $ inputCount i m)
calcPropensity (System m) (SndOrd r i1 i2 _) = r * (fromIntegral $ inputCount i1 m * inputCount i2 m)
calcPropensity (System m) (Assoc r i _) = let c = inputCount i m in r * (fromIntegral $ c * (c-1))
calcPropensity (System m) (Disassoc r i _) = r * (fromIntegral $ inputCount i m)

calcPropensities :: System -> [Reaction] -> [(Reaction, Propensity)]
calcPropensities s rs = zip rs $ fmap (calcPropensity s) rs

selectReaction :: [(Reaction, Propensity)] -> PropensitySum -> Maybe Reaction
selectReaction rps target = liftM fst . find gtTarget . scanl1 sumTuple $ rps 
                                where sumTuple (a,b) (c,d) = (c, d+b)
                                      gtTarget (_, x) = x > target

calcTimeInc :: PropensitySum -> Float -> Time
calcTimeInc propSum rnum = -log rnum / propSum

mapadj f = M.adjust f

react :: System -> Reaction -> System
react (System m) (ZeroOrd _ o) = System $ mapadj succ o m
react (System m) (FstOrd _ i o) = System $ mapadj succ o (mapadj pred i m) 
react (System m) (SndOrd r i1 i2 o) = System $ mapadj succ o (mapadj pred i2 (mapadj pred i1 m))
react (System m) (Assoc r i o) = System $ mapadj (succ.succ) o (mapadj (pred.pred) i m) 
react (System m) (Disassoc r i o) = react (System m) (Assoc r o i)


nextReaction' :: [Reaction] -> Float -> Float -> System -> Maybe (Reaction, Time)
nextReaction' rs rnum1 rnum2 s = pure (,) <*> selectReaction ps (rnum1 * propSum) <*> pure (calcTimeInc propSum rnum2)
                                    where ps = calcPropensities s rs
                                          propSum = sum (map snd ps)

doNextReaction :: (System, Time) -> [Reaction] -> Time -> Float -> Float -> Maybe (System, Time)
doNextReaction st@(s, now) rs end rnum1 rnum2 = nextReaction' rs rnum1 rnum2 s >>= doReaction
                                                where doReaction (r, deltat) = if (now + deltat) <= end 
                                                                                    then Just (react s r, now + deltat) 
                                                                                    else Nothing

run :: RandomGen g => (System, Time) -> [Reaction] -> Time -> Rand g [(System, Time)]
run st rs stopTime = liftM2 doNextReaction' getRandom getRandom >>= stopIfNoReaction
                        where stopIfNoReaction = maybe (return []) (\x -> liftM2 (:) (return x) (run x rs stopTime))
                              doNextReaction' = doNextReaction st rs stopTime

tabulateOutput :: [(System, Time)] -> String
tabulateOutput = unlines . (map $ show . M.toList . getCounts . fst)
                    where getCounts (System m) = m

c1 = Chemical "A"
c2 = Chemical "B"
r1 = FstOrd 0.5 c1 c2
r2 = FstOrd 0.1 c2 c1
rs = [r1, r2]

s = System (M.fromList [(c1, 100), (c2, 300)])

main :: IO ()
main = do 
        let output = tabulateOutput $ evalRand (run (s, 0.0) rs 100.0) (mkStdGen 1)
        putStr output
