{-# LANGUAGE TemplateHaskell #-}
module Data.Record5 where

import Data.Delimited.Field (Field)
import Data.Time (Day)

-- | What kind of characters are valid in a name? Oh my, who knows! Unicode is
-- | such a huge character set, and names are so different. Gonna leave this one
-- | wide open.
newtype Name = Name Field

data Gender
  = Male | Female
  deriving (Eq, Bounded, Ord, Enum)

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
