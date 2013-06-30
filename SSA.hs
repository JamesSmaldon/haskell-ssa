import qualified Data.Map as M
import Control.Monad

-- make things a bit more readable
type Name = String
type Count = Int
type Propensity = Float

data Chemical = Chemical Name deriving (Show, Ord, Eq)
data Reaction = Reaction { inputs :: [Chemical], outputs :: [Chemical], rate :: Float } deriving (Show)
data System = System (M.Map Chemical Count)

calcPropensity :: System -> Reaction -> Propensity
calcPropensity (System cmap) r = fromIntegral iprod * rate r
                                    where f = \i c -> c * M.findWithDefault 0 i cmap
                                          iprod = foldr f 0 (inputs r)

calcPropensities :: System -> [Reaction] -> [(Reaction, Propensity)]
calcPropensities s rs = zip rs $ fmap (calcPropensity s) rs

sumPropensities :: [Propensity] -> Propensity
sumPropensities = foldr (+) 0 

normalisePropensities :: [Propensity] -> [Propensity]
normalisePropensities rps = fmap (flip (/) totalP) rps
                                where totalP = sumPropensities rps

--selectReaction :: [(Reaction, Propensity)] -> Float -> Reaction
--selectReaction rps = normalisePropensities (fmap snd rps) 
