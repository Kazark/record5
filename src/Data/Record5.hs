{-# LANGUAGE TemplateHaskell #-}
module Data.Record5 where

import           Control.Category ((.))
import qualified Control.Isomorphism.Partial as PIso
import           Control.Isomorphism.Partial hiding (element)
import           Control.Isomorphism.Partial.TH
import           Data.Char (isAlpha)
import           Data.Colour (Colour)
import           Data.Colour.Names (readColourName)
import           Data.Time (Day)
import           Prelude hiding ((.), (<$>), (*>), (<*), (<*>), pure)
-- Since we need to parse and print data in the same format, we're going to use
-- invertible syntax descriptions, guaranteeing that our parser and printer will
-- always be in sync (though a good ol' "parse is left-inverse to print"
-- property-based test also goes a long way in that direction, if invertible
-- syntax is not available for whatever reason). If you want to understand this
-- technique thoroughly, the paper that introduced it is Tillmann Rendel and
-- Klaus Ostermann. Invertible Syntax Descriptions: Unifying Parsing and Pretty
-- Printing. In Proc. of Haskell Symposium, 2010.
-- https://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf
-- If you have a basic understanding of Haskell or parser and printer
-- combinators, it is not a particularly difficult read (though knowing just the
-- very basics of category theory helps too). The high-level idea is not
-- difficult to grasp at all: we want to describe the _syntax_, as we would with
-- BNF, and decouple this from saying _what we are going to do with the syntax_
-- (parse it, print it, format it as BNF). There is another Haskell
-- library---perhaps a little more thoroughly baked---that takes a somewhat
-- different tact, called `syntax`, but I haven't taken the time to get familiar
-- with it. Is this a little bit of overkill for the problem at hand? Yeah,
-- probably. But c'mon! Let's have some fun here!
import           Text.Syntax
-- If we cared about efficiency, maybe we shouldn't use naive... but in a little
-- breakable toy, whatever.
import           Text.Syntax.Parser.Naive
import           Text.Syntax.Printer.Naive

data Delimiter
  = Comma | Pipe | Space
  deriving (Eq, Bounded, Ord, Enum)

delim :: Delimiter -> Char
delim Comma = ','
delim Pipe = ','
delim Space = ' '

instance Show Delimiter where
  -- This is a standard extension,
  show Comma = ".csv"
  -- but these two are not, as far as I know.
  show Pipe = ".psv"
  show Space = ".ssv"

-- | A little trick I learned from:
-- | https://stackoverflow.com/a/53645493/834176
-- | to ensure that the parser and printer stay in sync.
-- | We could use invertible syntax descriptions here, but this demonstrates the
-- | fact that there are lighter-weight ways of keeping your parser and printer
-- | in sync for certain specialized cases. Also, do not get confused! There are
-- | _two_ syntaxes relevant to delimiters! One is the syntax of the file
-- | extension; the other, internal to the contents of the file. It will help
-- | avoid confusion if we don't use invertible syntax descriptions for what is
-- | external to the file's contents.
parseDelim :: String -> Maybe Delimiter
parseDelim s = lookup s tab where
  -- If this were a large datatype or frequently used, we might want to ensure
  -- this table was only calculated once. But it's neither large nor frequently
  -- used.
  tab = [(show x, x) | x <- [minBound..maxBound]]


data Gender = Male | Female

-- A shockingly rich color type
type Color = Colour Double -- haha, thanks Brits, very convenient

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

-- It says "define isomorphisms", but these are really partial isomorphisms.
-- The basic idea of the invertible syntax descriptions paper is: in Haskell we
-- have parser libraries, and printer combinator libraries. Their semantics are
-- very similar, yet we have to define them separately, because the functors
-- involved are covariant in one case and contravariant in the other. But if we
-- used partial isomorphisms instead---which, crudely stated, are a pair of
-- partial functions inverse to one another so that we can go in both
-- directions, and no longer favor a variance---then we can pull it off. This
-- means you have to write a bunch of boilerplate, which they boil away with
-- template Haskell. Yes, I pulled template Haskell into this tiny project.
-- Sorry.
$(defineIsomorphisms ''Gender)
$(defineIsomorphisms ''Record5)

-- | I've always found it more convenient to use in this form, than in the way
-- | Tillman & Ostermann define it.
element :: Eq a => a -> Iso a ()
element = inverse . PIso.element

delimSyntax :: Syntax d => Delimiter -> d ()
delimSyntax delimiter = element (delim delimiter) <$> token

genderSyntax :: Syntax d => d Gender
genderSyntax =
  let m = (male . element 'M') <$> token
      f = (female . element 'F') <$> token
  in m <|> f

nameSyntax :: Syntax d => d String
nameSyntax = many $ subset isAlpha <$> token

colorSyntax :: Syntax d => d Color
colorSyntax = _

dateSyntax :: Syntax d => d Day
dateSyntax = _

-- | We'll make this simple and say:
-- | 1. No matter which delimiter you are using, we expect exactly one to
-- |    separate two fields.
-- | 2. We do not expect any of the fields to contain an active delimiter.
-- | 3. We do not support mixing delimiters, including having extra spaces
-- |    around pipes or commas.
-- |
recordSyntax :: Syntax d => Delimiter -> d Record5
recordSyntax delimiter =
  let d = delimSyntax delimiter
  in record5
     <$> nameSyntax <* d
     <*> nameSyntax <* d
     <*> genderSyntax <* d
     <*> colorSyntax <* d
     <*> dateSyntax
