module Utils (
  spaces,
  line,
  just,
  nothing
) where

import Text.Parsec (Stream, ParsecT, skipMany1, char, noneOf, endOfLine,
                    manyTill)

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany1 $ char ' '

line :: Stream s m Char => ParsecT s u m String
line = noneOf "\n" `manyTill` endOfLine

just :: Functor f => f x -> f (Maybe x)
just = fmap Just

nothing :: Functor f => f x -> f (Maybe y)
nothing = fmap $ const Nothing
