{-# LANGUAGE TemplateHaskell #-}
module Data.Record5 where

import Data.Colour (Colour)
import Data.Time (Day)

data Gender
  = Male | Female
  deriving (Eq, Bounded, Ord, Enum)

-- | An impressively rich color type
-- | `Double` is an implementation detail here; we just want to think in terms
-- | of a color. Conveniently, we can add an alias since apparently the library
-- | is British. :P
type Color = Colour Double

data Record5
            -- Technically, String is too big of a type for a name, if we wanted
            -- to be precise. Considering the fact that no language I know of
            -- makes modeling precise subtypes of strings straightforward
            -- (though perhaps refinement types would be good for this?) I'm not
            -- going to overthink it here.
  = Record5 { lastName :: String
            , firstName :: String
            , gender :: Gender
            , favoriteColor :: Color
            , dateOfBirth :: Day }
