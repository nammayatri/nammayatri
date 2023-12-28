module Alchemist.Generator.Haskell.DomainHandler (generateDomainHandler) where

-- import Alchemist.DSL.Parser.API hiding (figureOutImports)
import Alchemist.DSL.Syntax.API
import Alchemist.Generator.Haskell.Servant (handlerFunctionText, handlerSignature)
-- import Alchemist.Utils

import Alchemist.GeneratorCore
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import Kernel.Prelude hiding (replicateM)

generateDomainHandler :: Apis -> Code
generateDomainHandler input =
  generateCode generatorInput
  where
    generatorInput :: GeneratorInput
    generatorInput =
      GeneratorInput
        { _ghcOptions = ["-Wno-orphans", "-Wno-unused-imports"],
          _extensions = [],
          _moduleNm = "Domain.Action.UI." <> T.unpack (_moduleName input),
          _simpleImports = allSimpleImports,
          _qualifiedImports = allQualifiedImports,
          _codeBody = generateCodeBody mkCodeBody input
        }
    qualifiedModuleName = T.unpack ("Domain.Action.UI." <> _moduleName input)

    allSimpleImports :: [String]
    allSimpleImports =
      [ "EulerHS.Prelude hiding (id)",
        "Servant",
        "Tools.Auth",
        "Data.OpenApi (ToSchema)",
        "API.Types.UI." <> T.unpack (_moduleName input)
      ]

    allQualifiedImports :: [String]
    allQualifiedImports =
      nub $
        preventSameModuleImports $
          (T.unpack <$> _imports input)
            <> defaultQualifiedImport

    preventSameModuleImports :: [String] -> [String]
    preventSameModuleImports = filter (\x -> not (qualifiedModuleName `isInfixOf` x))

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Kernel.Prelude", "Domain.Types.Person", "Domain.Types.Merchant", "Environment", "Kernel.Types.Id"]

mkCodeBody :: ApisM ()
mkCodeBody = do
  input <- ask
  let seperator = newLine *> newLine
  onNewLine $
    intercalateA seperator (map handlerFunctionDef (_apis input))
  where
    isAuthPresent :: ApiTT -> Bool
    isAuthPresent apiT = case _authType apiT of
      Just NoAuth -> False
      _ -> True

    handlerFunctionDef :: ApiTT -> ApisM ()
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerSignature apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> (if length allTypes > 1 then " -> " else " ") <> "Environment.Flow " <> last allTypes
       in tellM $
            T.unpack $
              functionName <> (if isAuthPresent apiT then " :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)" else " ::") <> handlerTypes
                <> "\n"
                <> functionName
                <> " = error \"Logic yet to be decided\""
