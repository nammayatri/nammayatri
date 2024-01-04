module Alchemist.Generator.Haskell.TransformerFunctions
  ( generateTransformerFunctions,
  )
where

import qualified Alchemist.DSL.Syntax.Transformer as ST
import Control.Lens ((^.))
import Data.Char (isLower)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, nub, sort)
import qualified Data.Text as T
import Kernel.Prelude

-- import GHC.IO (unsafePerformIO)

defaultMonad :: Text
defaultMonad = "Monad m"

whiteSpace :: Text
whiteSpace = " "

whiteSpaceChar :: Char
whiteSpaceChar = ' '

qualifiedFunctionPrefix :: Text
qualifiedFunctionPrefix = "_"

moduleNamePrefix :: String
moduleNamePrefix = "module Beckn.OnDemand.Transformer."

-- monadVars :: [Text]
-- monadVars = [" m", " m r"]

generateTransformerFunctions :: ST.Transformers -> String
generateTransformerFunctions input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    <> "\n\n"
    <> moduleNamePrefix
    <> T.unpack (input ^. ST.moduleName)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (nub $ makeImport <$> getImportList <> defaultQualifiedImport)
    <> "\n\n"
    <> T.unpack (generateFunctions (input ^. ST.monads) (input ^. ST.functions) (input ^. ST.imports))
  where
    makeImport :: String -> String
    makeImport x = "import qualified " <> x

    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude hiding (id)", "Kernel.Utils.Common (type (:::))"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Kernel.Types.Id"] -- "Environment"
    getImportList :: [String]
    getImportList = sort . filter (/= "") . map (T.unpack . snd) . HM.toList $ input ^. ST.imports

generateFunctions :: [Text] -> [ST.TransformerTT] -> HM.HashMap Text Text -> Text
generateFunctions globalMonads functionList importMap = T.unlines $ concatMap processFunction functionList
  where
    processFunction :: ST.TransformerTT -> [Text]
    processFunction (ST.TransformerTT name fromTypes paramNames toType outputTypeBindings pureMapping impureMapping) =
      [ createFunctionDefinition name globalMonads fromTypes importMap toType,
        name <> whiteSpace <> T.intercalate whiteSpace paramNames <> " = do"
      ]
        <> map
          ("  " <>)
          ( createPureMappings importMap pureMapping
              <> createImpureMappings importMap impureMapping
              <> ["pure $"]
              <> map
                ("  " <>)
                ( [toQualified importMap toType]
                    <> ["  { " <> T.intercalate ",\n        " (map (\(x, y) -> x <> " = " <> y) outputTypeBindings) <> "\n      }"]
                )
          )

createFunctionDefinition :: Text -> [Text] -> [Text] -> HM.HashMap Text Text -> Text -> Text
createFunctionDefinition name allMonads fromTypes importMap toType =
  let qualifiedMonads = map (toQualified importMap) allMonads
      qualifiedInputTypes = map (toQualified importMap) fromTypes
      monadVar = getMonadVar
      qualifiedOutputType = toQualified importMap toType
   in name <> " :: (" <> defaultMonad <> ", " <> T.intercalate ", " qualifiedMonads <> ") => " <> T.intercalate " -> " qualifiedInputTypes <> " -> " <> monadVar <> qualifiedOutputType
  where
    getMonadVar :: Text
    getMonadVar = case allMonads of
      [] -> ""
      _ -> "m "

createPureMappings :: HM.HashMap Text Text -> [(Text, Text)] -> [Text]
createPureMappings importMap = map (\(x, y) -> "let " <> x <> " = " <> mkQualified importMap y)

createImpureMappings :: HM.HashMap Text Text -> [(Text, Text)] -> [Text]
createImpureMappings importMap = map (\(x, y) -> x <> " <- " <> mkQualified importMap y)

mkQualified :: HM.HashMap Text Text -> Text -> Text
mkQualified importMap str =
  let firstWord' = T.takeWhile (/= whiteSpaceChar) str
      rest = fromMaybe "" (T.stripPrefix firstWord' str)
      firstWord = fromMaybe firstWord' (T.stripPrefix qualifiedFunctionPrefix firstWord')
      mbPrefix = HM.lookup firstWord importMap
   in -- val = unsafePerformIO $ putStrLn $ "firstWord: " ++ show qualifiedFirstWord

      case mbPrefix of
        Nothing -> firstWord <> rest -- <> show val
        Just prefix
          | T.null prefix -> firstWord <> rest -- <> show val
          | otherwise -> prefix <> "." <> firstWord <> rest -- <> show val

toQualified :: HM.HashMap Text Text -> Text -> Text
toQualified _ "" = ""
toQualified mp key
  -- to handle HasFlowEnv m r '["something" ::: Type] types of monad
  | "\'" `T.isPrefixOf` key = "\'" <> toQualified mp (T.tail key)
  | "\"" `T.isPrefixOf` key && "\"" `T.isSuffixOf` key = key
  | ":::" == key = key
  -- to handle list types.
  | "[" `T.isPrefixOf` key && "]" `T.isSuffixOf` key =
    let key' = fromMaybe key (T.stripPrefix "[" key)
        key'' = fromMaybe key' (T.stripSuffix "]" key')
     in "[" <> toQualified mp key'' <> "]"
  -- to handle brackets in types.
  | "(" `T.isPrefixOf` key && ")" `T.isSuffixOf` key =
    let key' = fromMaybe key (T.stripPrefix "(" key)
        key'' = fromMaybe key' (T.stripSuffix ")" key')
     in "(" <> toQualified mp key'' <> ")"
  -- to handle monad variables in types.
  | T.length key == 1 && isLower (T.head key) = key
  -- split complex types based on space.
  | whiteSpace `T.isInfixOf` key =
    let firstWord = T.takeWhile (/= whiteSpaceChar) key
        rest = T.tail $ fromMaybe whiteSpace (T.stripPrefix firstWord key)
        res1 = toQualified mp firstWord
        res2 = toQualified mp rest
     in res1 <> whiteSpace <> res2
  | otherwise =
    let prefix = HM.lookup key mp & fromMaybe (error $ "No imports found for the type/function : " <> key)
     in if T.null prefix
          then key
          else prefix <> "." <> key
