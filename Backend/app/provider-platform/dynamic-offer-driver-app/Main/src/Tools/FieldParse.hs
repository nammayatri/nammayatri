{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.FieldParse
  ( parseFieldM,
    parseFieldWithDefaultM,
    mkFieldParserWithDefault,
    mkFieldParserWithDefaultDebug,
    ParserWithDefault,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import Kernel.Prelude hiding (Type)
import Kernel.Types.Common
import Kernel.Utils.Common
import Language.Haskell.TH

parseFieldM :: (Monad m, Log m, FromJSON a) => Text -> Text -> Text -> Maybe A.Value -> m (Maybe a)
parseFieldM table field entityId = \case
  Just val -> case A.fromJSON val of
    A.Success x -> pure $ Just x
    A.Error err -> do
      logError $ "Entity field parsing failed for table: " <> table <> "; field: " <> field <> "; entityId: " <> entityId <> "; err: " <> show err <> "; default values will be used"
      pure Nothing
  Nothing -> pure Nothing

type ParserWithDefault a = a -> A.Object -> A.Parser ([String], a)

parseFieldWithDefaultM ::
  forall m a.
  (Monad m, Log m) =>
  Text ->
  Text ->
  Text ->
  a ->
  ParserWithDefault a ->
  Maybe A.Value ->
  m a
parseFieldWithDefaultM table field entityId def parser = \case
  Just val -> case A.parse (A.withObject (T.unpack field) $ parser def) val of
    A.Success (failedRecords, x) -> do
      unless (null failedRecords) $ do
        logDebug $ "Entity field records parsing failed for table:" <> table <> "; field: " <> field <> "; entityId: " <> entityId <> "; failedRecords: " <> show failedRecords <> "; default values will be used"
      pure x
    A.Error err -> do
      logError $ "Entity field parsing failed for table: " <> table <> "; field: " <> field <> "; entityId: " <> entityId <> "; err: " <> show err <> "; default values will be used"
      pure def
  Nothing -> pure def

parseMandatoryRecordHelper :: A.FromJSON a => A.Object -> A.Key -> A.Parser (Maybe String, Maybe a)
parseMandatoryRecordHelper obj key = do
  res <- obj A..:? key
  let failedRecord = case res of
        Nothing -> Just (show key)
        Just _ -> Nothing
  pure (failedRecord, res)

parseOptionalRecordHelper :: A.FromJSON a => A.Object -> A.Key -> A.Parser (Maybe String, Maybe a)
parseOptionalRecordHelper obj key = do
  res <- obj A..:? key
  pure (Nothing, res)

coerseName :: Name -> Name
coerseName = mkName . nameBase

-- | Will generate string for simple testing in repl. Using: putStrLn testSplice
mkFieldParserWithDefaultDebug :: Name -> Q [Dec]
mkFieldParserWithDefaultDebug name = do
  decs <- mkFieldParserWithDefault name
  testFunc <- mkTestSplice decs
  pure $ decs <> testFunc

mkTestSplice :: [Dec] -> Q [Dec]
mkTestSplice decs = do
  let fnName = mkName "testSplice"
  let fnSig = SigD fnName (ConT ''String)
  let fnBody = FunD fnName [Clause [] (NormalB . LitE . StringL $ pprint decs) []]
  return [fnSig, fnBody]

mkFieldParserWithDefault :: Name -> Q [Dec]
mkFieldParserWithDefault name = do
  recordsData <- reifyFields name
  let funName = mkName $ "parse" <> nameBase name <> "WithDefault"
      defName = mkName "def"
      objName = mkName "obj"
      parseRecordRes recordData = mkName $ (nameBase recordData.recordName) <> "'"
      failedRecordRes recordData = mkName $ (nameBase recordData.recordName) <> "Failed"
      recordsParseStmt =
        recordsData <&> \recordData -> do
          let keyLit = LitE . StringL . nameBase $ recordData.recordName
          let recordParseExp =
                if recordData.isMaybeType
                  then VarE 'parseOptionalRecordHelper `AppE` VarE objName `AppE` keyLit
                  else VarE 'parseMandatoryRecordHelper `AppE` VarE objName `AppE` keyLit
          BindS (TupP [VarP $ failedRecordRes recordData, VarP $ parseRecordRes recordData]) recordParseExp
      failedRecordDec = NormalB $ VarE 'catMaybes `AppE` ListE (VarE . failedRecordRes <$> recordsData)
      failedRecordsName = mkName "failedRecords"
      failedRecordStmt = LetS [ValD (VarP failedRecordsName) failedRecordDec []]
      resName = mkName "res"
      resBody = NormalB $ do
        RecConE (coerseName name) $
          recordsData <&> \recordData -> do
            let defFieldExpr = VarE recordData.recordName `AppE` VarE defName
                valFieldExpr = VarE $ parseRecordRes recordData
                fieldExpr =
                  if recordData.isMaybeType
                    then VarE 'mplus `AppE` valFieldExpr `AppE` defFieldExpr
                    else VarE 'fromMaybe `AppE` defFieldExpr `AppE` valFieldExpr
            (recordData.recordName, fieldExpr)
      resStmt = LetS [ValD (VarP resName) resBody []]
      returnStmt = NoBindS $ VarE 'pure `AppE` TupE [Just $ VarE failedRecordsName, Just $ VarE resName]
      funBody = NormalB $ DoE Nothing $ recordsParseStmt <> [failedRecordStmt, resStmt, returnStmt]
      funClause = Clause [VarP defName, VarP objName] funBody []
      funType = ConT (mkName "ParserWithDefault") `AppT` ConT name
  pure [SigD funName funType, FunD funName [funClause]]

data RecordData = RecordData
  { recordName :: Name,
    isMaybeType :: Bool,
    mbRecordType :: Type
  }

reifyFields :: Name -> Q [RecordData]
reifyFields name = do
  let nameStr = nameBase name
  mbTypeName <- lookupTypeName nameStr
  typeName <- case mbTypeName of
    Nothing -> fail $ nameStr <> " should be type name"
    Just n -> pure n
  typeInfo <- reify typeName
  case typeInfo of
    TyConI dec -> do
      constructor <- case dec of
        DataD _ _ _ _ [constructor] _ -> pure constructor
        NewtypeD _ _ _ _ constructor _ -> pure constructor
        _ -> fail $ nameStr <> " should be data type with one constructor"
      case constructor of
        RecC _ records -> forM records $ \(recordName, _, recordType) -> do
          case recordType of
            AppT (ConT maybeType) _ | maybeType == ''Maybe -> do
              pure
                RecordData
                  { recordName = recordName,
                    isMaybeType = True,
                    mbRecordType = recordType
                  }
            _ -> do
              pure
                RecordData
                  { recordName = recordName,
                    isMaybeType = False,
                    mbRecordType = AppT (ConT ''Maybe) recordType
                  }
        _ -> fail $ nameStr <> " should contain records"
    _ -> fail $ nameStr <> " should be type name"
