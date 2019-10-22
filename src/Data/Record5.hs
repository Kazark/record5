module Data.Record5
  ( Gender(..), Record5(..)
  , byGenderDescThenLastNameAsc, byDOBAsc, byLastNameDesc
  ) where

import Data.Delimited.Field (Field)
import Data.List (sortOn)
import Data.Time (Day)

data Gender
  = Male | Female
  deriving Eq

-- | Females before males
instance Ord Gender where
  compare Female Male = LT
  compare Male Female = GT
  compare _ _ = EQ

-- | Name and color fields have been left open-ended, because what is the proper
-- | way to constrain them? I did look into the `colour` library, and it was
-- | cool, but did not have symmetric parsing and printing of color names. :(
-- | When you understand your domain clearly, it is good to express it to the
-- | compiler clearly; when you do not, it is best to constrain it as little as
-- | you can, until you understand it better. Here, the only constraints we
-- | place on the name and color fields, is that they not contain delimiters.
data Record5
  = Record5 { lastName :: Field
            , firstName :: Field
            , gender :: Gender
            , favoriteColor :: Field
            , dateOfBirth :: Day }

newtype InvOrd a = InvOrd a deriving Eq

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

instance Ord a => Ord (InvOrd a) where
  compare (InvOrd a) (InvOrd b) = invertOrdering $ compare a b

byGenderDescThenLastNameAsc :: [Record5] -> [Record5]
byGenderDescThenLastNameAsc = sortOn (\r -> (gender r, InvOrd $ lastName r))

byDOBAsc :: [Record5] -> [Record5]
byDOBAsc = sortOn (InvOrd . dateOfBirth)

byLastNameDesc :: [Record5] -> [Record5]
byLastNameDesc = sortOn lastName
