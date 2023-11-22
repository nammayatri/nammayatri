module Alchemist.Generator.Haskell.Servant where

import Alchemist.AST.API
import Kernel.Prelude

generateServantCode :: APIAst -> String
generateServantCode = show
