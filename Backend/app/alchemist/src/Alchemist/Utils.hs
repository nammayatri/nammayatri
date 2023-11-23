module Alchemist.Utils where

import Data.List (intercalate, nub)
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import Kernel.Prelude hiding (hPutStr)
import System.IO

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
    replaceOrKeep word =
      if '.' `elem` word
        then word
        else maybe (if word `elem` ["", ")", "(", " "] then word else error "Type not determined") (\x -> x <> "." <> word) (getQualifiedImport word)

figureOutImports :: [String] -> [String]
figureOutImports fieldTypes =
  nub $ filter (not . null) $ concatMap (map (extractUptoLastDot . makeTypeQualified)) extractWords
  where
    extractWords = splitWhen (`elem` typeDelimiter) <$> fieldTypes
    extractUptoLastDot str =
      let parts = splitOn "." str
       in if length parts > 1
            then intercalate "." (init parts)
            else str

getQualifiedImport :: String -> Maybe String
getQualifiedImport = \case
  "Text" -> Just "Data.Text"
  "Maybe" -> Just "Data.Maybe"
  "Id" -> Just "Beckn.Types"
  "TimeOfDay" -> Just "Beckn.Types"
  "Int" -> Just "Data.Int"
  _ -> Nothing
