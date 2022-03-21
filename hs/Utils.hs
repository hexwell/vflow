module Utils (
  StringParser,
  spaces,
  line,
  just,
  nothing
) where

import Text.Parsec (Parsec, Stream, ParsecT, skipMany, char, noneOf, endOfLine,
                    manyTill)

type StringParser u = Parsec String u

type Parser s u m a = Stream s m Char => ParsecT s u m a
type FunctorOfXToFunctorOfMaybeY f x y = Functor f => f x -> f (Maybe y)

spaces ::  Parser s u m ()
spaces = skipMany $ char ' '

line :: Parser s u m String
line = noneOf "\n" `manyTill` endOfLine

just :: FunctorOfXToFunctorOfMaybeY f x x
just = fmap Just

nothing :: FunctorOfXToFunctorOfMaybeY f x y
nothing = fmap $ const Nothing
