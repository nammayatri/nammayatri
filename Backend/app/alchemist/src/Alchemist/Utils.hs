module Alchemist.Utils where

import Control.Lens.Combinators
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Lens (key, _Value)
import Data.Char (toUpper)
import Data.List (intercalate, nub)
import qualified Data.List as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import qualified Data.Text as T
import Kernel.Prelude hiding (Show, fromString, hPutStr, toString, traceShowId, try)
import System.IO

writeToFile :: FilePath -> String -> IO ()
writeToFile filename content = do
  withFile filename WriteMode $ \handle_ -> do
    hPutStr handle_ content

typeDelimiter :: String
typeDelimiter = "() []"

makeTypeQualified :: Maybe [String] -> Object -> String -> String
makeTypeQualified dList obj str = concatMap replaceOrKeep (split (whenElt (`elem` typeDelimiter)) str)
  where
    defaultTypeImports :: String -> Maybe String
    defaultTypeImports tp = case tp of
      "Text" -> Just "Kernel.Prelude"
      "Maybe" -> Just "Kernel.Prelude"
      "Double" -> Just "Kernel.Prelude"
      "TimeOfDay" -> Just "Kernel.Prelude"
      "Day" -> Just "Data.Time.Calendar"
      "Int" -> Just "Kernel.Prelude"
      "Bool" -> Just "Kernel.Prelude"
      "Id" -> Just "Kernel.Types.Id"
      _ -> Nothing

    getQualifiedImport :: String -> Maybe String
    getQualifiedImport tk = case preview (ix "imports" . key (fromString tk) . _String) obj of
      Just t -> Just t
      Nothing -> defaultTypeImports tk

    replaceOrKeep :: String -> String
    replaceOrKeep word =
      if '.' `elem` word
        then word
        else
          if isJust dList && L.elem word (fromJust dList)
            then "Domain.Types." ++ word ++ "." ++ word
            else maybe (if word `elem` ["", ")", "(", " ", "[", "]"] then word else error $ T.pack ("\"" ++ word ++ "\" type not determined")) (\x -> x <> "." <> word) (getQualifiedImport word)

figureOutImports :: [String] -> [String]
figureOutImports fieldTypes =
  nub $ filter (not . null) $ concatMap (map (extractUptoLastDot)) extractWords
  where
    extractWords = splitWhen (`elem` typeDelimiter) <$> fieldTypes
    extractUptoLastDot str =
      let pp = splitOn "." str
       in if length pp > 1
            then intercalate "." (init pp)
            else str

-- Helper function to capitalize a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

_String :: Prism' Value String
_String = _Value . prism (String . T.pack) (\v -> case v of String s -> Right $ T.unpack s; _ -> Left v)
