module Alchemist.Generator.Haskell.DomainType where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import Kernel.Prelude

defaultImports :: [String]
defaultImports = ["Kernel.Prelude"]

generateDomainType :: TableDef -> String
generateDomainType tableDef =
  "module " ++ moduleName ++ " where\n\n"
    ++ intercalate "\n" (map (\i -> "import qualified " ++ i ++ " as " ++ i) (removeDefaultImports moduleName $ imports tableDef))
    ++ "\n"
    ++ intercalate "\n" (map ("import " ++) defaultImports)
    ++ "\n\ndata "
    ++ tableNameHaskell tableDef
    ++ " = "
    ++ tableNameHaskell tableDef
    ++ "\n  { "
    ++ intercalate "\n  , " (map fieldDefToHaskell (fields tableDef))
    ++ "\n  }\n  deriving (Generic, Show)\n"
  where
    moduleName = "Domain.Types." ++ tableNameHaskell tableDef

removeDefaultImports :: String -> [String] -> [String]
removeDefaultImports moduleName = filter ((/=) moduleName) . filter (`notElem` defaultImports)

-- Convert FieldDef to Haskell field
fieldDefToHaskell :: FieldDef -> String
fieldDefToHaskell fieldDef =
  fieldName fieldDef ++ " :: " ++ haskellType fieldDef
