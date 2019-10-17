{-# LANGUAGE TemplateHaskell #-}
module Data.Record5.Syntax (printRecord5, parseRecord5) where

import           Control.Category ((.))
import qualified Control.Isomorphism.Partial as PIso
import           Control.Isomorphism.Partial (Iso, (<$>), inverse, subset)
import           Control.Isomorphism.Partial.TH
import           Data.Char (isAlpha)
import           Data.Colour.Names (readColourName)
import           Data.Time (parseTimeM, formatTime, defaultTimeLocale)
import           Data.Record5
import           Prelude hiding ((.), (<$>), (<*), (<*>))
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
import           Text.Syntax (Syntax, (<*), (<*>), (<|>), many, token)
-- If we cared about efficiency, maybe we shouldn't use naive... but in a little
-- breakable toy, whatever.
import           Text.Syntax.Parser.Naive (parseM)
import           Text.Syntax.Printer.Naive (printM)

delimChar :: Delimiter -> Char
delimChar Comma = ','
delimChar Pipe = ','
delimChar Space = ' '

-- | Ultimately I'd like to use more precise types for color and date of birth,
-- | but because the invertible syntax library doesn't seem to have got much
-- | production use after its initial suggestion in the paper, it is rather
-- | primitive and doesn't support lifting of parsers and printers defined other
-- | ways into it. Therefore I'll just read stuff in a more raw form, and
-- | validate afterwards.
data RawRecord5
  = RawRecord5 { _lastName :: String
               , _firstName :: String
               , _gender :: Gender
               , _rawFavoriteColor :: String
               , _rawDateOfBirth :: String }

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
$(defineIsomorphisms ''RawRecord5)

-- | I've always found it more convenient to use in this form, than in the way
-- | Tillman & Ostermann define it.
element :: Eq a => a -> Iso a ()
element = inverse . PIso.element

delimSyntax :: Syntax d => Delimiter -> d ()
delimSyntax delimiter = element (delimChar delimiter) <$> token

genderSyntax :: Syntax d => d Gender
genderSyntax =
  let m = (male . element 'M') <$> token
      f = (female . element 'F') <$> token
  in m <|> f

nameSyntax :: Syntax d => d String
nameSyntax = many $ subset isAlpha <$> token

rawFieldSyntax :: Syntax d => Delimiter -> d String
rawFieldSyntax delimiter =
  many $ subset (not . (== (delimChar delimiter))) <$> token

-- | We'll make this simple and say:
-- | 1. No matter which delimiter you are using, we expect exactly one to
-- |    separate two fields.
-- | 2. We do not expect any of the fields to contain an active delimiter.
-- | 3. We do not support mixing delimiters, including having extra spaces
-- |    around pipes or commas.
rawRecordSyntax :: Syntax d => Delimiter -> d RawRecord5
rawRecordSyntax delimiter =
  let d = delimSyntax delimiter
  in rawRecord5
     <$> nameSyntax <* d
     <*> nameSyntax <* d
     <*> genderSyntax <* d
     <*> rawFieldSyntax delimiter <* d
     <*> rawFieldSyntax delimiter

dateFormat :: String
dateFormat = "%m/%d/%Y"

validate :: RawRecord5 -> IO Record5
validate (RawRecord5 lastN firstN gender' fColor dateOB) = do
  color <- readColourName fColor
  date <- parseTimeM False defaultTimeLocale dateFormat dateOB
  return $ Record5 lastN firstN gender' color date

-- We encapsulate the use of invertible syntax descriptions within this module
parseRecord5 :: Delimiter -> String -> IO Record5
parseRecord5 delim text = do
  raw <- parseM (rawRecordSyntax delim) text
  validate raw

rarify :: Record5 -> RawRecord5
rarify (Record5 lastN firstN gender' fColor dateOB) =
  let date = formatTime defaultTimeLocale dateFormat dateOB
  in RawRecord5 lastN firstN gender' (show fColor) date

-- We encapsulate the use of invertible syntax descriptions within this module
printRecord5 :: Delimiter -> Record5 -> IO String
printRecord5 delim = printM (rawRecordSyntax delim) . rarify
