module Alchemist.Generator.Haskell.TransformerFunctions
  ( generateTransformerFunctions,
  )
where

import qualified Alchemist.DSL.Syntax.Transformer as ST
import Control.Lens ((^.))
import Data.Char (isLower)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, nub, sort)
import Data.List.Extra ((!?))
import qualified Data.Text as T
import Kernel.Prelude

-- import GHC.IO (unsafePerformIO)

whiteSpace :: Text
whiteSpace = " "

whiteSpaceChar :: Char
whiteSpaceChar = ' '

qualifiedFunctionPrefix :: Text
qualifiedFunctionPrefix = "_"

multipleImportInfix :: Text
multipleImportInfix = "#"

maybePrefix :: Text
maybePrefix = "Maybe "

moduleNamePrefix :: String
moduleNamePrefix = "module Beckn.OnDemand.Transformer."

defaultImports :: [String]
defaultImports = ["EulerHS.Prelude hiding (id)", "Kernel.Utils.Common (type (:::))"]

defaultQualifiedImport :: [String]
defaultQualifiedImport = ["Kernel.Prelude", "Kernel.Types.Id", "BecknV2.OnDemand.Utils.Common"] -- "Environment"

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

    getImportList :: [String]
    getImportList = do
      let imports' = (concatMap snd . HM.toList) $ input ^. ST.imports
      sort . filter (/= "") . map T.unpack $ imports'

generateFunctions :: [Text] -> [ST.TransformerTT] -> HM.HashMap Text [Text] -> Text
generateFunctions globalMonads functionList importMap = T.unlines $ concatMap processFunction functionList
  where
    processFunction :: ST.TransformerTT -> [Text]
    processFunction (ST.TransformerTT name extraMonads fromTypes paramNames toType outputTypeBindings pureMapping impureMapping) =
      let allMonads = nub $ globalMonads <> extraMonads
       in [ createFunctionDefinition name allMonads fromTypes importMap toType,
            name <> whiteSpace <> T.intercalate whiteSpace paramNames <> " = do"
          ]
            <> map
              ("  " <>)
              ( createPureMappings importMap pureMapping
                  <> createImpureMappings importMap impureMapping
                  <> createOutput importMap toType outputTypeBindings (not $ null allMonads)
              )

createFunctionDefinition :: Text -> [Text] -> [Text] -> HM.HashMap Text [Text] -> Text -> Text
createFunctionDefinition name allMonads fromTypes importMap toType =
  let qualifiedMonads = map (toQualified importMap) allMonads
      qualifiedInputTypes = map (toQualified importMap) fromTypes
      monadVar = getMonadVar
      qualifiedOutputType = toQualified importMap toType
   in name <> " :: (" <> T.intercalate ", " qualifiedMonads <> ") => " <> T.intercalate " -> " qualifiedInputTypes <> " -> " <> monadVar qualifiedMonads <> " (" <> qualifiedOutputType <> ")"
  where
    getMonadVar :: [Text] -> Text
    getMonadVar = \case
      [] -> ""
      _ -> "m"

createPureMappings :: HM.HashMap Text [Text] -> [(Text, Text)] -> [Text]
createPureMappings importMap = map (\(x, y) -> "let " <> x <> " = " <> mkFunctionsQualified importMap y)

createImpureMappings :: HM.HashMap Text [Text] -> [(Text, Text)] -> [Text]
createImpureMappings importMap = map (\(x, y) -> x <> " <- " <> mkFunctionsQualified importMap y)

createOutput :: HM.HashMap Text [Text] -> Text -> [(Text, Text)] -> Bool -> [Text]
createOutput importMap toType outputTypeBindings anyMonad = do
  let strippedToType = stripReturnType toType
  let returnValue = toQualified importMap strippedToType <> " { " <> T.intercalate ", " (map (\(x, y) -> x <> " = " <> y) outputTypeBindings) <> " }"
  -- <> ["  { " <> T.intercalate ",\n        " (map (\(x, y) -> x <> " = " <> y) outputTypeBindings) <> "\n      }"]
  if maybePrefix `T.isPrefixOf` toType
    then textForMaybeToType returnValue
    else [(if anyMonad then "pure $ " else "") <> returnValue]
  where
    textForMaybeToType returnValue =
      ["let returnData = " <> returnValue]
        <> ["let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData"]
        <> ["if allNothing"]
        <> ["  then " <> (if anyMonad then "pure " else "") <> "Nothing"]
        <> ["  else " <> (if anyMonad then "pure $ " else "") <> "Just returnData"]

mkFunctionsQualified :: HM.HashMap Text [Text] -> Text -> Text
mkFunctionsQualified importMap str
  -- to handle list types.
  | "[" `T.isPrefixOf` str && "]" `T.isSuffixOf` str =
    let str' = fromMaybe str (T.stripPrefix "[" str)
        str'' = fromMaybe str' (T.stripSuffix "]" str')
     in "[" <> mkFunctionsQualified importMap str'' <> "]"
  -- to handle brackets in types.
  | "(" `T.isPrefixOf` str && ")" `T.isSuffixOf` str =
    let str' = fromMaybe str (T.stripPrefix "(" str)
        str'' = fromMaybe str' (T.stripSuffix ")" str')
     in "(" <> mkFunctionsQualified importMap str'' <> ")"
  | whiteSpace `T.isInfixOf` str =
    let firstWord = T.takeWhile (/= whiteSpaceChar) str
        rest = T.tail $ fromMaybe whiteSpace (T.stripPrefix firstWord str)
        res1 = mkFunctionsQualified importMap firstWord
        res2 = mkFunctionsQualified importMap rest
     in res1 <> whiteSpace <> res2
  | qualifiedFunctionPrefix `T.isPrefixOf` str = do
    let splitList = T.splitOn multipleImportInfix $ T.tail str
        (functionName, idx') = (head splitList, fromMaybe "0" $ splitList !? 1)
        idx = read $ T.unpack idx'
        matchedValue = HM.lookup functionName importMap & fromMaybe (error $ "No imports found for the function : " <> functionName)
    if length matchedValue <= idx
      then error ("Imports for the functionName: " <> functionName <> " is less than given index: " <> idx')
      else do
        let prefix = matchedValue !! idx
        if T.null prefix
          then functionName
          else prefix <> "." <> functionName
  | otherwise = str

stripReturnType :: Text -> Text
stripReturnType str
  | "Maybe " `T.isPrefixOf` str = T.stripPrefix "Maybe " str & (stripReturnType . fromMaybe str)
  | ")" `T.isSuffixOf` str = do
    let prefix = T.takeWhile (/= '(') str
        str' = T.stripPrefix prefix str <&> T.tail & fromMaybe (error $ "Mismatched parenthesis in the type " <> str)
        str'' = T.stripSuffix ")" str' & fromMaybe (error $ "Mismatched parenthesis in the type " <> str')
    stripReturnType str''
  | otherwise = str

toQualified :: HM.HashMap Text [Text] -> Text -> Text
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
  | otherwise = do
    let splitList = T.splitOn multipleImportInfix key
        (type', idx') = (head splitList, fromMaybe "0" $ splitList !? 1)
        idx = read $ T.unpack idx'
        matchedValue = HM.lookup type' mp & fromMaybe (error $ "No imports found for the type: " <> type')
    if length matchedValue <= idx
      then error ("Imports for the type': " <> type' <> " is less than given index: " <> idx')
      else do
        let prefix = matchedValue !! idx
        if T.null prefix
          then type'
          else prefix <> "." <> type'
