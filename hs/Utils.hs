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

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany $ char ' '

line :: Stream s m Char => ParsecT s u m String
line = noneOf "\n" `manyTill` endOfLine

just :: Functor f => f x -> f (Maybe x)
just = fmap Just

nothing :: Functor f => f x -> f (Maybe y)
nothing = fmap $ const Nothing
