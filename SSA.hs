import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Random
import Control.Applicative
import Debug.Trace
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
calcPropensity (System cmap) r = (fromIntegral iprod) * rate r
                                    where f = \i c -> c * M.findWithDefault 0 i cmap
                                          iprod = foldr f 1 (inputs r)

calcPropensities :: System -> [Reaction] -> [(Reaction, Propensity)]
calcPropensities s rs = zip rs $ fmap (calcPropensity s) rs

sumPropensities :: [Propensity] -> Propensity
sumPropensities = foldr (+) 0 

selectReaction :: [(Reaction, Propensity)] -> Float -> Maybe Reaction
selectReaction rps target = liftM fst . find gtTarget . scanl1 sumTuple $ rps 
                                where sumTuple (a,b) (c,d) = (c, d+b)
                                      gtTarget (_, x) = x > target

calcTimeInc :: Float -> Float -> Float 
calcTimeInc propSum rnum = -log(rnum) / propSum

react :: System -> Reaction -> System
react (System m) r = System $ mapadj (flip (+) 1) newInputs (outputs r)
                        where newInputs = mapadj (flip (-) 1) m (inputs r)
                              mapadj f = foldr (M.adjust f)

type STW g = StateT (System, Float) (RandT g (Writer [String]))

nextReaction' :: [Reaction] -> Float -> Float -> System -> Maybe (Reaction, Float)
nextReaction' rs rnum1 rnum2 s = pure (,) <*> selectReaction ps rnum1 <*> pure (calcTimeInc propSum rnum2)
                                    where ps = calcPropensities s rs
                                          propSum = sumPropensities (map snd ps)

runReaction' :: (System, Float) -> Maybe (Reaction, Float) -> Maybe (System, Float)
runReaction' (s, now) Nothing = Nothing
runReaction' (s, now) (Just (r, deltat)) = Just (react s r, now + deltat)

runSystem' :: (System, Float) -> [Reaction] -> Float -> Float -> Maybe (System, Float)
runSystem' st@(s, now) rs rnum1 rnum2 = (liftM doReaction) . (nextReaction' rs rnum1 rnum2) $ s
                                            where doReaction (r, deltat) = (react s r, now + deltat)

nextReaction :: RandomGen g => System -> [Reaction] -> STW g (Maybe (Reaction, Float))
nextReaction s rs = do
                        let ps = calcPropensities s rs
                        let propsum = sumPropensities (map snd ps)
                        -- lift selectReaction into the Rand monad (returns Maybe (Reaction))
                        rct <- liftM (selectReaction ps) (getRandomR (0, propsum))
                        -- lift calcTimeInce into the Rand Monad
                        time <- liftM (calcTimeInc propsum) getRandom
                        -- Use maybe as an applicative to construct a tuple
                        return $ pure (,) <*> rct <*> Just time

runReaction :: RandomGen g => Maybe (Reaction, Float) -> STW g Bool
runReaction Nothing = return False
runReaction (Just (rct, next)) = do
                            (s, now) <- get 
                            put (react s rct, now + next)
                            return True

runSystem :: RandomGen g => Float -> [Reaction] -> STW g Bool
runSystem end rs = do
                        (s, now) <- get
                        rct <- nextReaction s rs
                        rctHappened <- runReaction rct
                        (s', next) <- get
                        return $ rctHappened && next < end

runSystem'' end rs = iterateUntil ((==) False) (runSystem end rs)
                        

c1 = Chemical "A"
c2 = Chemical "B"
r1 = Reaction [c1] [c2] 0.1
r2 = Reaction [c2] [c1] 0.3
rs = [r1, r2]

s = System (M.fromList [(c1, 100), (c2, 300)])
