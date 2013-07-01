import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Random
import Data.Maybe

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
selectReaction' _ 0.0 _ = Nothing
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
react (System m) r = System $ mapadj (flip (+) 1) newInputs (outputs r)
                        where newInputs = mapadj (flip (-) 1) m (inputs r)
                              mapadj f = foldr (M.adjust f)

type RandWriter g a = MaybeT (RandT g (Writer [String])) a

nextReaction :: RandomGen g => System -> [Reaction] -> RandWriter g (System, Float)
nextReaction s rs = do
                        let ps = calcPropensities s rs
                        let propsum = sumPropensities (map snd ps)
                        tr <- getRandom
                        let time = calcTimeInc tr propsum
                        r <- getRandomR (0, propsum)
                        rct <- return $ selectReaction ps r
                        lift . lift $ tell ["Reaction " ++ show (fromJust rct) ++ " occurred."]
                        return $ (react s (fromJust rct), time)

{-runSystem :: RandomGen g => (System,Float) -> Float -> [Reaction] -> RandWriter g (System, Float)
runSystem (s, now) end rs
    | now >= end = return (s, end)
    | otherwise = do 
                    r <- runMaybeT $ nextReaction s rs
                    case r of
                        Just (s', t) -> runSystem (s',t) end rs
                        Nothing -> return (s, now)

c1 = Chemical "A"
c2 = Chemical "B"
r1 = Reaction [c1] [c2] 0.1
r2 = Reaction [c2] [c1] 0.3
r3 = Reaction [c1,c1] [c2] 0.4
rs = [r1, r2, r3]

s = System (M.fromList [(c1, 100), (c2, 300)])-}
