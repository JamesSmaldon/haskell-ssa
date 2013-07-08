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
type Propensity = Float
type PropensitySum = Float
type Time = Float

data Chemical = Chemical Name deriving (Show, Ord, Eq)
data Reaction = Reaction { inputs :: [Chemical], outputs :: [Chemical], rate :: Float } deriving (Show)
data System = System (M.Map Chemical Count) deriving (Show)

calcPropensity :: System -> Reaction -> Propensity
calcPropensity (System cmap) r = fromIntegral iprod * rate r
                                    where f i c = c * M.findWithDefault 0 i cmap
                                          iprod = foldr f 1 (inputs r)

calcPropensities :: System -> [Reaction] -> [(Reaction, Propensity)]
calcPropensities s rs = zip rs $ fmap (calcPropensity s) rs

selectReaction :: [(Reaction, Propensity)] -> PropensitySum -> Maybe Reaction
selectReaction rps target = liftM fst . find gtTarget . scanl1 sumTuple $ rps 
                                where sumTuple (a,b) (c,d) = (c, d+b)
                                      gtTarget (_, x) = x > target

calcTimeInc :: PropensitySum -> Float -> Time
calcTimeInc propSum rnum = -log rnum / propSum

react :: System -> Reaction -> System
react (System m) r = System $ mapadj (+ 1) newInputs (outputs r)
                        where newInputs = mapadj pred m (inputs r)
                              mapadj f = foldr (M.adjust f)

nextReaction' :: [Reaction] -> Float -> Float -> System -> Maybe (Reaction, Time)
nextReaction' rs rnum1 rnum2 s = pure (,) <*> selectReaction ps rnum1 <*> pure (calcTimeInc propSum rnum2)
                                    where ps = calcPropensities s rs
                                          propSum = sum (map snd ps)

doNextReaction :: (System, Time) -> [Reaction] -> Time -> Float -> Float -> Maybe (System, Time)
doNextReaction st@(s, now) rs end rnum1 rnum2 = nextReaction' rs rnum1 rnum2 s >>= doReaction
                                                where doReaction (r, deltat) = if (now + deltat) <= end 
                                                                                    then Just (react s r, now + deltat) 
                                                                                    else Nothing

run :: RandomGen g => (System, Float) -> [Reaction] -> Time -> Rand g [(System, Time)]
run st rs stopTime = liftM2 doNextReaction' getRandom getRandom >>= stopIfNoReaction
                        where stopIfNoReaction = maybe (return []) (\x -> liftM2 (:) (return x) (run x rs stopTime))
                              doNextReaction' = doNextReaction st rs stopTime

c1 = Chemical "A"
c2 = Chemical "B"
r1 = Reaction [c1] [c2] 0.1
r2 = Reaction [c2] [c1] 0.3
rs = [r1, r2]

s = System (M.fromList [(c1, 100), (c2, 300)])
