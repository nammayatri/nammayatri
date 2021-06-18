module Beckn.Types.Validation.RegexParsecEngine (regexEngine) where

import Beckn.Types.Validation.Regex
import EulerHS.Prelude hiding (Alt, Any)
import qualified Text.Parsec as P

regexEngine :: RegexEngine
regexEngine regex = either (const False) (const True) . P.parse (toParser regex *> P.eof) ""

{-# ANN module ("HLint: Use mapM Found" :: String) #-}

toParser :: Regex -> P.Parsec Text () ()
toParser (Ch match) = void $ P.satisfy (matchChar match)
toParser (Not match) = void $ P.satisfy (not . matchChar match)
toParser (Any matches) = void $ P.satisfy $ opChar (||) (const False) (map matchChar matches)
toParser (NotAny matches) = void $ P.satisfy $ opChar (&&) (const True) (map matchChar matches)
toParser (Alt regexes) = asum (map toParser regexes)
toParser (Seq regexes) = void $ sequence (map toParser regexes)
toParser (Opt regex) = P.optional (toParser regex)
toParser (Many regex) = void $ P.many (toParser regex)

opChar :: (Bool -> Bool -> Bool) -> (Char -> Bool) -> [(Char -> Bool)] -> Char -> Bool
opChar op = foldr (\f s x -> f x `op` s x)
