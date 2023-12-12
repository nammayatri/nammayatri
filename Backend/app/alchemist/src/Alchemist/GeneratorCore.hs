{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Alchemist.GeneratorCore where

import Alchemist.DSL.Syntax.API
import Alchemist.DSL.Syntax.Storage
import Control.Lens hiding (noneOf)
import Control.Monad.RWS (MonadReader (local), RWS, execRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Data.Functor (void)
import Data.String (IsString (..))
import Data.String.Builder (Builder, build, literal)
import Prelude

data GeneratorInput = GeneratorInput
  { _ghc_options :: [String],
    _qualifiedImports :: [String],
    _simpleImports :: [String],
    _codeBody :: String
  }

$(makeLenses ''GeneratorInput)

type BuilderM = RWS GeneratorInput Builder ()

newtype HaskellCode = HaskellCode Builder

instance IsString (BuilderM ()) where
  fromString = tell . literal

surrounded :: BuilderM () -> BuilderM () -> BuilderM () -> BuilderM ()
surrounded begin end am =
  begin *> am *> end

withinParens :: BuilderM () -> BuilderM ()
withinParens = surrounded "( " " )"

withinSpaces :: BuilderM () -> BuilderM ()
withinSpaces = surrounded " " " "

quoted :: BuilderM () -> BuilderM ()
quoted = surrounded "'" "'"

followedBy :: BuilderM () -> BuilderM () -> BuilderM ()
followedBy = (*>)

onNewline :: BuilderM () -> BuilderM ()
onNewline = followedBy (tell "\n")

intercalateA :: Applicative m => m () -> [m a] -> m ()
intercalateA sep = \case
  [] -> pure ()
  [x] -> void x
  (x : xs) ->
    x *> sep *> intercalateA sep xs

generateCode :: GeneratorInput -> HaskellCode
generateCode generatorCore =
  HaskellCode $
    snd $
      execRWS code generatorCore ()

code :: BuilderM ()
code = do
  pragmaClause
  simpleImportClause
  qualifiedImportClause
  codeBodyClause
  where
    pragmaClause :: BuilderM ()
    pragmaClause = error "TODO" --intercalateA (tell $ literal "\n") $ map (tell . literal) $ view ghc_options --error "TODO"
    simpleImportClause :: BuilderM () -- intercalateA $ fmap tell _ghc_options
    simpleImportClause = error "TODO"

    qualifiedImportClause :: BuilderM ()
    qualifiedImportClause = error "TODO"

    codeBodyClause :: BuilderM ()
    codeBodyClause = error "TODO"
