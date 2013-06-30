import qualified Data.Map as M
import Control.Monad

-- make things a bit more readable
type Name = String
type Count = Int
type Propensity = Float

data Chemical = Chemical Name deriving (Show, Ord, Eq)
data Reaction = Reaction { inputs :: [Chemical], outputs :: [Chemical], rate :: Float } deriving (Show)
data System = System (M.Map Chemical Count) deriving (Show)

modSystem :: System -> (M.Map Chemical Count -> M.Map Chemical Count) -> System
modSystem (System map) f = System (f map)

getMap :: System -> M.Map Chemical Count
getMap (System map) = map

calcPropensity :: System -> Reaction -> Propensity
calcPropensity (System cmap) r = fromIntegral iprod * rate r
                                    where f = \i c -> c * M.findWithDefault 0 i cmap
                                          iprod = foldr f 1 (inputs r)

calcPropensities :: System -> [Reaction] -> [(Reaction, Propensity)]
calcPropensities s rs = zip rs $ fmap (calcPropensity s) rs

sumPropensities :: [Propensity] -> Propensity
sumPropensities = foldr (+) 0 

selectReaction' :: [(Reaction, Propensity)] -> Float -> Float -> Maybe Reaction
selectReaction' [] target current = Nothing
selectReaction' ((r,p):xs) target current
                    | next > target = Just r
                    | otherwise = selectReaction' xs target next
                        where next = current + p

selectReaction :: [(Reaction, Propensity)] -> Float -> Maybe Reaction
selectReaction rps s = selectReaction' rps s 0.0

calcTimeInc :: Float -> Float -> Float 
calcTimeInc rnum propSum = -log(rnum) / propSum

react :: System -> Reaction -> System
react (System m) r = System $ foldr (\x -> M.adjust ((+) 1) x) ((foldr (\x -> M.adjust ((-) 1) x) m (outputs r))) (inputs r)

--runSystem :: System -> [Reactions] -> Float -> Writer System

{-c1 = Chemical "A"
c2 = Chemical "B"
r1 = Reaction [c1] [c2] 0.1
r2 = Reaction [c2] [c1] 0.3
r3 = Reaction [c1,c1] [c2] 0.4
rs = [r1, r2, r3]

s = System (M.fromList [(c1, 100), (c2, 300)])-}
