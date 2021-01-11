{-|
Module: Ex10
Description: Exercise 10
-}

-- This lists what this module exports. Don't change this!
module Ex10
    (
    mapMaybes,
    composeMaybe,
    foldMaybe, 
    applyBinaryMaybe,
    collectMaybes,
    Pet(..),
    Person(..),
    Robot(..),
    Organization(..),
    robotCompany,
    mapF
    )
where

-------------------------------------------------------------------------------
-- * Task 1: Practice with maybe
-------------------------------------------------------------------------------
import Data.Maybe


mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes _ [Nothing] = [Nothing]
mapMaybes x (Nothing:z) = (mapMaybes x z)
mapMaybes x (y:z) = [fmap x y] ++ (mapMaybes x z)


composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe x y = (\g -> y (fromJust (x g)))

foldMaybe :: (b -> a -> Maybe b) -> b -> [a] -> Maybe b
foldMaybe f y [z] = (f y z) 
foldMaybe f y (z:r) = foldMaybe f (fromJust (f y z)) r


applyBinaryMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
applyBinaryMaybe f Nothing _ = Nothing
applyBinaryMaybe f _ Nothing = Nothing
applyBinaryMaybe f x y = Just (f (fromJust x ) (fromJust y))


convert :: [Maybe a] -> Maybe [a]
convert [Nothing] = Nothing
convert [a] = Just [fromJust a]

collectMaybes :: [Maybe a] -> Maybe [a]
collectMaybes (Nothing:b) = collectMaybes b 
collectMaybes (a:b) = Just (fromJust(convert [a]) ++ fromJust (collectMaybes b ))


-------------------------------------------------------------------------------
-- * Task 2: The Eq and Functor Typeclasses
-------------------------------------------------------------------------------

data Pet = Cat String Int -- a cat has a name and an age
         | Dog String Int -- a dog has a name and an age
         | Ants           -- an ant farm is just an ant farm
         deriving Show
instance Eq Pet where
    -- Here, we'll need to make Pet an instance of the Eq type class
    -- function for this datatype. Two pets are equal if they are the
    -- same animal and have the same name.  The two animals don't have
    -- to have the same age. All ants are the same.
    (==) (Cat name1 age1) (Cat name2 age2) = name1 == name2
    (==) (Dog name1 age1) (Dog name2 age2) = name1 == name2 
    (==) (Ants) (Ants) = True 
    (==) _ _ = False

-- example
fluffy = Cat "Fluffy" 3
myAnts = Ants

-- Here, we'll need to make Organization an instance of the Eq
-- *and* Functor typeclass.
data Person  = Person String Float      -- name, salary
             deriving (Show, Eq)
data Robot   = Robot Int                -- identifier
             deriving (Show, Eq)
data Organization p = Individual p             -- organization of one
                    | Team p [Organization p]  -- team leader, and list of sub-orgs
                    deriving Show
instance Functor Organization where
    fmap f (Individual p) = Individual(f p)
    fmap f (Team leader []) = (Team (f leader) [])
    fmap f (Team leader orgs) = (Team (f leader) (map (fmap f) orgs))
     -- TODO

-- robot organization:
robot1   = Robot 1
robotOrg = Individual robot1

-- example:
owner   = Person "Janet" 100000
cto     = Person "Larry"  90000
cfo     = Person "Mike"   90000
intern  = Person "Sam"    40000
company = Team owner [Team cto [Individual intern],
                      Individual cfo]

-- Use a call to `fmap` to turn the example value `company`
-- into an organization with the same structure, but populated
-- entirely by robots. 
robotize :: Person -> Robot
robotize p = Robot 0

robotCompany = fmap robotize company


-- Write the generalization of the `mapMaybes` function that would work
-- for any functors, not just `Maybe`.
mapF :: Functor f => (a -> b) -> [f a] -> [f b]
mapF f [] = []
mapF f (x:xs) = [fmap f x] ++ mapF f xs

