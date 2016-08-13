-- WeIrDStRiNgCaSe

import Data.Char (toUpper, toLower)
import Data.String (words, unwords)
import Data.Functor ((<$>))

toWeirdCase :: String -> String
toWeirdCase str =
  unwords (trans <$> words str)
  where
    trans = zipWith ($) (cycle [toUpper, toLower])
