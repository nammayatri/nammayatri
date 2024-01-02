module Alchemist.Utils where

import Alchemist.DSL.Syntax.API (ApiType (..))
import Alchemist.DSL.Syntax.Storage (FieldRelation (..))
import Control.Lens.Combinators
import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Lens (key, _Value)
import Data.Char (isLower, toLower, toUpper)
import Data.List (intercalate, nub)
import qualified Data.List as L
import Data.List.Split (split, splitOn, splitWhen, whenElt)
import qualified Data.Text as T
import Kernel.Prelude hiding (Show, fromString, hPutStr, toString, traceShowId, try)
import System.Directory (createDirectoryIfMissing)
import System.IO
import Text.Regex.TDFA ((=~))

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"

startsWithLower :: String -> Bool
startsWithLower (x : _) = isLower x
startsWithLower _ = False

writeToFile :: FilePath -> FilePath -> String -> IO ()
writeToFile directoryPath fileName content = do
  createDirectoryIfMissing True directoryPath
  withFile (directoryPath ++ "/" ++ fileName) WriteMode $ \handle_ -> do
    hPutStr handle_ content

typeDelimiter :: String
typeDelimiter = "() []"

isMaybeType :: String -> Bool
isMaybeType tp = L.isPrefixOf "Maybe " tp || L.isPrefixOf "Data.Maybe.Maybe " tp || L.isPrefixOf "Kernel.Prelude.Maybe " tp

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
  "Meters" -> Just "Kernel.Types.Common"
  "HighPrecMeters" -> Just "Kernel.Types.Common"
  "Kilometers" -> Just "Kernel.Types.Common"
  "HighPrecMoney" -> Just "Kernel.Types.Common"
  _ -> Nothing

removeOccurrence :: String -> String -> String
removeOccurrence remove str = T.unpack $ T.replace (T.pack remove) (T.pack "") (T.pack str)

regexExec :: String -> String -> Maybe String
regexExec str pattern' = do
  let match = str =~ pattern' :: (String, String, String, [String])
  listToMaybe $ filter (/= "") $ snd4 match
  where
    snd4 (_, _, _, x) = x

getFieldRelationAndHaskellType :: String -> Maybe (FieldRelation, String)
getFieldRelationAndHaskellType str = do
  let patternOneToOne' :: String = "^Domain\\.Types\\..*\\.(.*)$"
      patternMaybeOneToOne' :: String = "^Kernel.Prelude.Maybe Domain\\.Types\\..*\\.(.*)$"
      patternOneToMany' :: String = "^\\[Domain\\.Types\\..*\\.(.*)\\]$"
      fieldRelationAndHaskellType =
        case (regexExec str patternOneToOne', regexExec str patternMaybeOneToOne', regexExec str patternOneToMany') of
          (Just haskellType, Nothing, Nothing) -> Just (OneToOne, haskellType)
          (Nothing, Just haskellType, Nothing) -> Just (MaybeOneToOne, haskellType)
          (Nothing, Nothing, Just haskellType) -> Just (OneToMany, haskellType)
          (_, _, _) -> Nothing
  if isJust fieldRelationAndHaskellType && validatePattern
    then fieldRelationAndHaskellType
    else Nothing
  where
    validatePattern =
      case splitOn "." str of
        [_, _, third, fourth] -> third == fourth
        _ -> False

-- makeTypeQualified (Maybe Module name)
makeTypeQualified :: Maybe String -> Maybe [String] -> Maybe [String] -> String -> Object -> String -> String
makeTypeQualified moduleName excludedList dList defaultImportModule obj str = concatMap replaceOrKeep (split (whenElt (`elem` typeDelimiter)) str)
  where
    getQualifiedImport :: String -> Maybe String
    getQualifiedImport tk = case preview (ix "imports" . key (fromString tk) . _String) obj of
      Just t -> Just t
      Nothing -> defaultTypeImports tk

    replaceOrKeep :: String -> String
    replaceOrKeep word =
      if '.' `elem` word || ',' `elem` word
        then word
        else
          if isJust moduleName && isJust excludedList && word `elem` (fromJust excludedList)
            then defaultImportModule ++ fromJust moduleName ++ "." ++ word
            else
              if isJust dList && L.elem word (fromJust dList)
                then defaultImportModule ++ word ++ "." ++ word
                else maybe (if word `elem` ["", ")", "(", " ", "[", "]", "e"] then word else error $ T.pack ("\"" ++ word ++ "\" type not determined")) (\x -> x <> "." <> word) (getQualifiedImport word)

figureOutImports :: [String] -> [String]
figureOutImports fieldTypes =
  nub $ filter (not . null) $ concatMap (map (extractUptoLastDot)) extractWords
  where
    extractWords = splitWhen (`elem` typeDelimiter) <$> fieldTypes
    extractUptoLastDot str =
      let pp = splitOn "." str
       in if length pp > 1
            then intercalate "." (init pp)
            else
              if startsWithLower str
                then ""
                else str

-- Helper function to capitalize a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

-- Helper function to lowercase the first letter of a string
lowercaseFirstLetter :: String -> String
lowercaseFirstLetter "" = ""
lowercaseFirstLetter (x : xs) = toLower x : xs

_String :: Prism' Value String
_String = _Value . prism (String . T.pack) (\v -> case v of String s -> Right $ T.unpack s; _ -> Left v)

_Bool :: Prism' Value Bool
_Bool = _Value . prism Bool (\v -> case v of Bool s -> Right s; _ -> Left v)
