-- Abstract Syntax Tree representation
module Alchemist.AST.API where

import Alchemist.DSL.Syntax.API

-- We can reuse Alchemist.DSL.Syntax types if they are sufficient
type APIAst = [APIEndpoint]
