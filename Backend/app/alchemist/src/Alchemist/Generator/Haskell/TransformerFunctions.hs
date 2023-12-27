module Alchemist.Generator.Haskell.TransformerFunctions
  ( generateTransformerFunctions,
  )
where

import qualified Alchemist.DSL.Syntax.Transformer as ST
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, nub)
import qualified Data.Text as T
import Kernel.Prelude

defaultMonad :: Text
defaultMonad = "Monad m"

moduleNamePrefix :: String
moduleNamePrefix = "module Beckn.OnDemand.Transfomer."

generateTransformerFunctions :: ST.Transformers -> String
generateTransformerFunctions input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    <> "\n\n"
    <> moduleNamePrefix
    <> T.unpack (input ^. ST.moduleName)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports) -- JAYPAL
    <> "\n\n"
    <> intercalate "\n" (nub $ makeImport <$> getImportList <> defaultQualifiedImport)
    <> "\n\n"
    <> T.unpack (generateFunctions (input ^. ST.monads) (input ^. ST.functions) (input ^. ST.imports))
  where
    makeImport :: String -> String
    makeImport x = "import qualified " <> x

    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude hiding (id)", "Servant", "Data.OpenApi (ToSchema)"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Environment", "Kernel.Types.Id"]

    getImportList :: [String]
    getImportList = map (T.unpack . snd) . HM.toList $ input ^. ST.imports

generateFunctions :: [Text] -> [ST.TransformerTT] -> HM.HashMap Text Text -> Text
generateFunctions globalMonads functionList importMap = T.unlines $ concatMap processFunction functionList
  where
    processFunction :: ST.TransformerTT -> [Text]
    processFunction (ST.TransformerTT name fromTypes paramNames toType outputTypeBindings pureMapping impureMapping) =
      [ createFunctionDefinition name globalMonads fromTypes importMap toType,
        name <> " " <> T.intercalate " " paramNames <> " = do"
      ]
        <> map
          ("  " <>)
          ( createPureMappings pureMapping
              <> createImpureMappings impureMapping
              <> ["pure $"]
              <> map
                ("  " <>)
                ( [getValue importMap toType <> "." <> toType]
                    <> ["  { " <> T.intercalate ",\n\t\t\t\t" (map (\(x, y) -> x <> " = " <> y) outputTypeBindings) <> "\n\t\t\t}"]
                )
          )

createFunctionDefinition :: Text -> [Text] -> [Text] -> HM.HashMap Text Text -> Text -> Text
createFunctionDefinition name allMonads fromTypes importMap toType =
  let qualifiedMonads = map (\monad -> getValue importMap monad <> "." <> monad) allMonads
      qualifiedInputTypes = map (\type' -> getValue importMap type' <> "." <> type') fromTypes
      monadVar = getMonadVar
      qualifiedOutputType = getValue importMap toType <> "." <> toType
   in name <> " :: (" <> defaultMonad <> ", " <> T.intercalate ", " qualifiedMonads <> ") => " <> T.intercalate " -> " qualifiedInputTypes <> " -> " <> monadVar <> qualifiedOutputType
  where
    getMonadVar :: Text
    getMonadVar = case allMonads of
      [] -> ""
      _ -> "m "

createPureMappings :: [(Text, Text)] -> [Text]
createPureMappings = map (\(x, y) -> "let " <> x <> " = " <> y)

createImpureMappings :: [(Text, Text)] -> [Text]
createImpureMappings = map (\(x, y) -> x <> " <- " <> y)

monadVars :: [Text]
monadVars = [" m", " m r"]

getValue :: HM.HashMap Text Text -> Text -> Text
getValue mp key =
  let key' = foldl' (\acc x -> fromMaybe acc (T.stripSuffix x acc)) key monadVars
   in HM.lookup key' mp & fromMaybe (error $ "No imports found for the type/function : " <> key')
