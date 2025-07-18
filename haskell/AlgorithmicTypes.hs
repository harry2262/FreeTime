

-- Enumeration Type 
-- deriving Show which tells GHC to autogenerate code for Thing to string to print
data Thing = Shoe | Ship | Car | Brush deriving Show  


shoe :: Thing
shoe = Shoe
-- functions with enumeration 

isSmall :: Thing -> Bool 
isSmall Shoe = True
isSmall Brush = True
isSmall _ = False

-- Beyond Enumerations 
data FailableDouble = Failure 
                      | Ok Double
                      deriving Show

-- Ok and Failure are Contructors for FailableDouble
--  You cannot have another type with same name of Contructors
ex0 = Failure

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x/y)

-- Data Contructors with more than one argument

data Person = Person String Int Thing 
  deriving Show


brent = Person "Brent" 23 Brush

-- Pattern Matching
-- pattern-matching is about taking apart a value by finding out which constructor it was built with. 
-- This information can be used as the basis for deciding what to do—indeed, in Haskell, 
-- this is the only way to make a decision.
foo (Failure) = 0
foo (Ok d) = d



-- usage of _ (underscore) in pattern-matching
--  Also Note the nesting of patterns (usage of Brush pattern)
checkFav (Person n _ Brush) = n++", You're my kind of person"
checkFav (Person n _ _) = n++", Your Favorite Thing is lame"

-- x@(pat) usage

getName p@(Person n _ _ ) = "The name field of (" ++ show p ++ ") is " ++ n



main = do
  print shoe
  print ex0
  print brent
