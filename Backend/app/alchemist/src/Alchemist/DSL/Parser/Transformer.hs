-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Alchemist.DSL.Parser.Transformer
  ( transformerParser,
  )
where

import qualified Alchemist.DSL.Syntax.Transformer as ST
-- import qualified Alchemist.Utils as U
import Control.Lens hiding (mapping, noneOf)
import Data.Aeson
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Array, _Object, _String)
import Data.Bool
import qualified Data.ByteString as BS
import Data.Char (toLower, toUpper)
import qualified Data.HashMap.Strict as HM
-- import Data.List.Split (splitWhen)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Kernel.Prelude hiding (toText)

defaultImportMap :: HM.HashMap Text Text
defaultImportMap =
  HM.fromList
    [ ("Id", "Kernel.Types.Id"),
      ("getId", "Kernel.Types.Id"),
      ("ShortId", "Kernel.Types.Id"),
      ("getShortId", "Kernel.Types.Id"),
      ("Maybe", ""), -- Already included in EulerHS.Prelude
      ("Just", ""), -- Already included in EulerHS.Prelude
      ("Nothing", "") -- Already included in EulerHS.Prelude
    ]

pureIdentifier :: Text
pureIdentifier = "~"

impureIdentifier :: Text
impureIdentifier = "/~"

transformerParser :: FilePath -> IO ST.Transformers
transformerParser filepath = do
  contents <- BS.readFile filepath
  case Yaml.decodeEither' contents of
    Left _ -> error "Not a valid Yaml"
    Right yml -> pure $ parseTransformers yml

parseTransformers :: Object -> ST.Transformers
parseTransformers obj =
  let moduleName = preview (ix "module" . _String) obj & fromMaybe (error "Module name is required")
      importObj = preview (ix "imports" . _Object) obj & fromMaybe (error "Imports are required")
      _imports = HM.fromList . map (\(k, v) -> (toText k, preview _String v & fromMaybe (error "Failed to parse imports"))) $ KM.toList importObj
      imports = HM.unionWith const defaultImportMap _imports
      transformerObj = preview (ix "transformer" . _Object) obj & fromMaybe (error "Failed to parse transformer")
      monads = preview (ix "monads" . _Array . to V.toList) transformerObj & convertToListOfText
      unParsedFunctions = preview (ix "transformers" . _Object) transformerObj & fromMaybe (error "Failed to parse transformers Key Object") & KM.toList
      functions = mapM parseSingleTransformer unParsedFunctions & fromMaybe (error "Failed to parse transformer functions")
   in ST.Transformers moduleName imports functions monads

convertToListOfText :: Maybe [Value] -> [Text]
convertToListOfText Nothing = []
convertToListOfText (Just val) = mapM (preview _String) val & fromMaybe (error "Failed to convert monads to Text")

parseSingleTransformer :: (Key, Value) -> Maybe ST.TransformerTT
parseSingleTransformer (key, value) = do
  obj <- preview _Object value
  let name = toText key
      params = preview (ix "params" . _Object) obj & fromMaybe (error "Transformer params is required") & KM.toList & map toTextPair
      fromTypes = map snd params
      paramNames = map fst params
      toType = preview (ix "toType" . _String) obj & fromMaybe (error "Transformer toType is required")
      mapping = preview (ix "mapping" . _Object) obj & fromMaybe (error "Transformer mapping is required") & KM.toList & map toTextPair
      outputTypeBindings = map (setFieldBindings toType) mapping
      impureMapping = map (setImpureBindings toType) $ filter (\(_, val) -> impureIdentifier `T.isPrefixOf` val) mapping
      pureMapping = map (setPureBindings toType) $ filter (\(_, val) -> not $ impureIdentifier `T.isPrefixOf` val) mapping
  pure $ ST.TransformerTT name fromTypes paramNames toType outputTypeBindings pureMapping impureMapping
  where
    toTextPair :: (Key, Value) -> (Text, Text)
    toTextPair (k, v) =
      let key' = toText k
          value' = preview _String v & fromMaybe (error "Transformer mappings value needs to be Text")
       in (key', value')

    setImpureBindings :: Text -> (Text, Text) -> (Text, Text)
    setImpureBindings toType' (key', val') =
      let outputTypePrefix = updateFirstChar toLower toType'
          val = fromMaybe val' (T.stripPrefix impureIdentifier val')
       in (outputTypePrefix <> updateFirstChar toUpper key', val)

    setPureBindings :: Text -> (Text, Text) -> (Text, Text)
    setPureBindings toType' (key', val') =
      let outputTypePrefix = updateFirstChar toLower toType'
          val = fromMaybe val' (T.stripPrefix pureIdentifier val')
       in (outputTypePrefix <> updateFirstChar toUpper key', val)

    setFieldBindings :: Text -> (Text, Text) -> (Text, Text)
    setFieldBindings toType' (key', _) =
      let outputTypePrefix = updateFirstChar toLower toType'
       in (key', outputTypePrefix <> updateFirstChar toUpper key')

    updateFirstChar :: (Char -> Char) -> Text -> Text
    updateFirstChar _ "" = ""
    updateFirstChar f xs = T.cons (f $ T.head xs) (T.tail xs)
