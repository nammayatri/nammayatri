module Beckn.Types.Validation.Regex where

import qualified Data.Ix as Ix
import qualified Data.Text as T
import EulerHS.Prelude hiding (Alt, Any)

data MatchChar
  = AnyChar
  | ExactChar Char
  | Range Char Char
  | MatchWithFunction Text (Char -> Bool)

data Regex
  = Ch MatchChar
  | Not MatchChar
  | Any [MatchChar]
  | NotAny [MatchChar]
  | Alt [Regex]
  | Seq [Regex]
  | Opt Regex
  | Many Regex

type RegexEngine = Regex -> Text -> Bool

matchChar :: MatchChar -> Char -> Bool
matchChar AnyChar = const True
matchChar (ExactChar c) = (== c)
matchChar (Range c1 c2) = Ix.inRange (c1, c2)
matchChar (MatchWithFunction _name p) = p

showMatchChar :: MatchChar -> Text
showMatchChar AnyChar = "."
showMatchChar (ExactChar c) = T.singleton c
showMatchChar (Range a b) = T.singleton a <> "-" <> T.singleton b
showMatchChar (MatchWithFunction name _) = ":" <> name <> ":"

showRegex :: Regex -> Text
showRegex (Ch match) = showMatchChar match
showRegex (Not match) = "[^" <> showMatchChar match <> "]"
showRegex (Any matches) = "[" <> T.concat (map showMatchChar matches) <> "]"
showRegex (NotAny matches) = "[^" <> T.concat (map showMatchChar matches) <> "]"
showRegex (Alt regexes) = "\\(" <> T.intercalate "\\|" (map showRegex regexes) <> "\\)"
showRegex (Seq regexes) = T.concat (map showRegex regexes)
showRegex (Opt regex) = "\\(" <> showRegex regex <> "\\)\\?"
showRegex (Many regex) = "\\(" <> showRegex regex <> "\\)*"

space, latinLC, latinUC, digit :: MatchChar
space = ExactChar ' '
latinLC = Range 'a' 'z'
latinUC = Range 'A' 'Z'
digit = Range '0' '9'

latin, alphanum, latinOrSpace :: [MatchChar]
latin = [Range 'a' 'z', Range 'A' 'Z']
alphanum = latin <> [digit]
latinOrSpace = latin <> [space]
