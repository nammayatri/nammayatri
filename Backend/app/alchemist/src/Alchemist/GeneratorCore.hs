{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Alchemist.GeneratorCore where

import Control.Lens hiding (noneOf)
import Control.Monad.RWS (RWS, execRWS)
import Control.Monad.Writer.Class (MonadWriter (tell))
import Data.Functor (void)
import Data.String (IsString (..))
import Data.String.Builder (Builder, build, literal)
import Prelude

type BuilderM a = RWS a Builder ()

newtype Code = Code Builder

instance Show Code where
  show (Code b) = build b

data GeneratorInput = GeneratorInput
  { _ghcOptions :: [String],
    _extensions :: [String],
    _moduleNm :: String,
    _simpleImports :: [String],
    _qualifiedImports :: [String],
    _codeBody :: Code
  }

$(makeLenses ''GeneratorInput)

instance IsString (BuilderM a ()) where
  fromString = tell . literal

surrounded :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a () -> BuilderM a () -> BuilderM a ()
surrounded begin end am =
  begin *> am *> end

surround :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a () -> BuilderM a ()
surround same am =
  same *> am *> same

withinCurls :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
withinCurls = surrounded "{ " " }"

withinParens :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
withinParens = surrounded "( " " )"

withinSpaces :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
withinSpaces = surrounded " " " "

quoted :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
quoted = surrounded "'" "'"

followedBy :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a () -> BuilderM a ()
followedBy = (*>)

comma :: forall a. IsString (BuilderM a ()) => BuilderM a ()
comma = tellM ","

space :: forall a. IsString (BuilderM a ()) => BuilderM a ()
space = tellM " "

withSpace :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
withSpace = followedBy space

replicateM :: forall a. IsString (BuilderM a ()) => Int -> BuilderM a () -> BuilderM a ()
replicateM n = void . sequence . replicate n

newLine :: forall a. IsString (BuilderM a ()) => BuilderM a ()
newLine = tellM "\n"

withSomeSpaces :: forall a. IsString (BuilderM a ()) => Int -> BuilderM a () -> BuilderM a ()
withSomeSpaces n = followedBy (replicateM n " ")

onNewLine :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
onNewLine = followedBy newLine

lineSpace :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
lineSpace = flip followedBy newLine

afterFewLines :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
afterFewLines = followedBy (replicateM 3 newLine)

mkImport :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
mkImport = followedBy (tellM "import ")

mkImportQ :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
mkImportQ = followedBy (tellM "import qualified ")

intercalateA :: forall a. IsString (BuilderM a ()) => BuilderM a () -> [BuilderM a ()] -> BuilderM a ()
intercalateA sep = \case
  [] -> pure ()
  [x] -> void x
  (x : xs) ->
    x *> sep *> intercalateA sep xs

mkGHCOptions :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
mkGHCOptions = surrounded begin end . withinSpaces
  where
    begin = tellM "{-# OPTIONS_GHC"
    end = tellM "#-}"

mkExtension :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
mkExtension = surrounded begin end . withinSpaces
  where
    begin = tellM "{-# LANGUAGE"
    end = tellM "#-}"

mkModuleName :: forall a. IsString (BuilderM a ()) => BuilderM a () -> BuilderM a ()
mkModuleName = surrounded begin end . withinSpaces
  where
    begin = tellM "module"
    end = tellM "where"

tellM :: String -> BuilderM a ()
tellM = tell . literal

tellCode :: Code -> BuilderM a ()
tellCode (Code code) = tell code

generateCodeBody :: forall a. IsString (BuilderM a ()) => BuilderM a () -> a -> Code
generateCodeBody builder state =
  Code $
    snd $
      execRWS builder state ()

generateCode :: GeneratorInput -> Code
generateCode generatorCore =
  Code $
    snd $
      execRWS code generatorCore ()
  where
    code :: BuilderM GeneratorInput ()
    code = do
      pragmaClause
      extensionClause
      moduleName'
      simpleImportClause
      qualifiedImportClause
      codeClause
      where
        moduleName' = view moduleNm >>= onNewLine . mkModuleName . tellM
        pragmaClause = view ghcOptions >>= lineSpace . intercalateA newLine . map (mkGHCOptions . tellM)
        extensionClause = view extensions >>= onNewLine . intercalateA newLine . map (mkExtension . tellM)
        simpleImportClause = view simpleImports >>= onNewLine . intercalateA newLine . map (mkImport . tellM)
        qualifiedImportClause = view qualifiedImports >>= onNewLine . intercalateA newLine . map (mkImportQ . tellM)
        codeClause = view codeBody >>= afterFewLines . tellM . show
