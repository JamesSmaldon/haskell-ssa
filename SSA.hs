import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Monad
import Control.Monad.Random
import Debug.Trace
import Control.Applicative
import Data.Maybe
import Data.Colour.Names
import Data.Colour
import Data.Accessor
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk

-- make things a bit more readable
type Name = String
type Count = Int
type Rate = Double
type Propensity = Double
type PropensitySum = Double
type Time = Double
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

calcTimeInc :: PropensitySum -> Double -> Time
calcTimeInc propSum rnum = -log rnum / propSum

mapadj f = M.adjust f

react :: System -> Reaction -> System
react (System m) (ZeroOrd _ o) = System $ mapadj succ o m
react (System m) (FstOrd _ i o) = System $ mapadj succ o (mapadj pred i m) 
react (System m) (SndOrd r i1 i2 o) = System $ mapadj succ o (mapadj pred i2 (mapadj pred i1 m))
react (System m) (Assoc r i o) = System $ mapadj (succ.succ) o (mapadj (pred.pred) i m) 
react (System m) (Disassoc r i o) = react (System m) (Assoc r o i)


nextReaction' :: [Reaction] -> Double -> Double -> System -> Maybe (Reaction, Time)
nextReaction' rs rnum1 rnum2 s = pure (,) <*> selectReaction ps (rnum1 * propSum) <*> pure (calcTimeInc propSum rnum2)
                                    where ps = calcPropensities s rs
                                          propSum = sum (map snd ps)

doNextReaction :: (System, Time) -> [Reaction] -> Time -> Double -> Double -> Maybe (System, Time)
doNextReaction st@(s, now) rs end rnum1 rnum2 = nextReaction' rs rnum1 rnum2 s >>= doReaction
                                                where doReaction (r, deltat) = if (now + deltat) <= end 
                                                                                    then Just (react s r, now + deltat) 
                                                                                    else Nothing

run :: RandomGen g => (System, Time) -> [Reaction] -> Time -> Rand g [(System, Time)]
run st rs stopTime = liftM2 doNextReaction' getRandom getRandom >>= stopIfNoReaction
                        where stopIfNoReaction = maybe (return []) (\x -> liftM2 (:) (return x) (run x rs stopTime))
                              doNextReaction' = doNextReaction st rs stopTime

namedTimeSeries :: [(System, Time)] -> [(Chemical, [(Time, Double)])]
namedTimeSeries xs = M.toList $ foldl' addTimes M.empty xs
                       where addTimes m2 (System m, time) = foldl' (insertDataPt time) m2 (M.toList m)   
                             insertDataPt time m3 (chem, cnt)  = M.insertWith (++) chem [(time, fromIntegral cnt)] m3



createPlotLines :: [(Chemical, [(Time, Double)])] -> [AlphaColour Double] -> [Either (Plot Time Double) (Plot Time Double)]
createPlotLines xs cols = map createPlotLine $ zip xs cols
                where createPlotLine ((Chemical n, ts),col) = Left $ toPlot $ plot_lines_style .> line_color ^= col
                                                           $ plot_lines_values ^= [ts]
                                                           $ plot_lines_title ^= n 
                                                           $ defaultPlotLines 

c1 = Chemical "A"
c2 = Chemical "B"
c3 = Chemical "C"
r1 = FstOrd 0.5 c1 c2
r2 = FstOrd 0.5 c2 c1
rs = [r1, r2]

s = System (M.fromList [(c1, 300), (c2, 0)])

chart nts = layout 
  where
    layout = layout1_title ^="SSA"
           $ layout1_plots ^= createPlotLines nts [opaque blue, opaque green]
           $ layout1_grid_last ^= False
           $ defaultLayout1

main :: IO ()
main = do 
        let output = namedTimeSeries $ evalRand (run (s, 0.0) rs 10000.0) (mkStdGen 1)
        renderableToWindow (toRenderable $ chart output) 640 480
