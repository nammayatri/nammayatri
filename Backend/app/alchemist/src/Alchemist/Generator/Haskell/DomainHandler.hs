module Alchemist.Generator.Haskell.DomainHandler where

-- import Alchemist.DSL.Parser.API hiding (figureOutImports)
import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
-- import Alchemist.Utils
import Data.List (intercalate, isInfixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateDomainHandler :: Apis -> String
generateDomainHandler input =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n"
    <> "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    <> "\n\n"
    <> "module Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (nub $ preventSameModuleImports $ makeImport <$> (T.unpack <$> _imports input) <> defaultQualifiedImport)
    <> "\n\n"
    -- <> T.unpack (generateHaskellTypes (_types input))
    <> T.unpack (T.intercalate "\n\n" (map handlerFunctionDef (_apis input)))
  where
    qualifiedModuleName = T.unpack ("Domain.Action.UI." <> _moduleName input)

    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude hiding (id)", "Servant", "Tools.Auth", "Data.OpenApi (ToSchema)", "API.Types.UI." <> T.unpack (_moduleName input)]

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

    makeImport :: String -> String
    makeImport x = "import qualified " <> x

    handlerFunctionDef :: ApiTT -> Text
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> " -> " <> "Environment.Flow " <> last allTypes
       in functionName <> " :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)" <> handlerTypes
            <> "\n"
            <> functionName
            <> " = error \"Logic yet to be decided\""
