module Alchemist.Utils where

import Data.List (nub)
import Data.List.Split (split, splitWhen, whenElt)
import Kernel.Prelude hiding (hPutStr)
import System.IO
import Text.Regex.TDFA ((=~))

writeToFile :: FilePath -> String -> IO ()
writeToFile filename content = do
  withFile filename WriteMode $ \handle_ -> do
    hPutStr handle_ content

typeDelimiter :: String
typeDelimiter = "() "

makeTypeQualified :: String -> String
makeTypeQualified str = concatMap replaceOrKeep (split (whenElt (`elem` typeDelimiter)) str)
  where
    replaceOrKeep :: String -> String
    replaceOrKeep word = fromMaybe (if '.' `elem` word || word `elem` ["", ")", "(", " "] then word else error "Type not determined") (getQualifiedImport word)

figureOutImports :: [String] -> [String]
figureOutImports fieldTypes =
  let extractWords = splitWhen (`elem` typeDelimiter) <$> fieldTypes
   in filter (not . null) $ concatMap (map makeTypeQualified) (nub extractWords)

findMatchingSqlType :: String -> Maybe String
findMatchingSqlType haskellType =
  case filter ((haskellType =~) . fst) defaultSQLTypes of
    [] -> Nothing
    ((_, sqlType) : _) -> Just sqlType

defaultSQLTypes :: [(String, String)]
defaultSQLTypes =
  [ ("Text", "CHARACTER(36)"),
    ("Id ", "Varchar(50)"),
    ("TimeOfDay", "UTCTime")
  ]

getQualifiedImport :: String -> Maybe String
getQualifiedImport = \case
  "Text" -> Just "Data.Text.Text"
  "Maybe" -> Just "Data.Maybe.Maybe"
  "Id" -> Just "Beckn.Types.Id"
  "TimeOfDay" -> Just "Beckn.Types.TimeOfDay"
  "Int" -> Just "Int"
  _ -> Nothing
