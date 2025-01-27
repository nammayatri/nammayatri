module Domain.Action.Dashboard.System (postSystemRunQuery, generateInsertQuery) where

import qualified API.Types.RiderPlatform.Management.System
import Data.Aeson
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.ByteString.Lazy as BSL
import Data.Char as DC
import qualified Data.Scientific as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Tuple.Extra as DTX
import qualified Data.Vector as DV
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Types (Query (Query))
import qualified Domain.Types.Merchant
import Domain.Types.UtilsTH
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as KP
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import qualified Storage.Beam.HotSpotConfig as HSC
import qualified Storage.Beam.Issue as ISU
import qualified Storage.Beam.MerchantConfig as MC
import qualified Storage.Beam.MerchantServiceConfig as MSC
import qualified Storage.Beam.MerchantServiceUsageConfig as MSUC
import qualified Storage.Beam.PayoutConfig as PC
import qualified Storage.Beam.PlaceBasedServiceConfig as PBSC
import qualified Storage.Beam.RideRelatedNotificationConfig as RRNC
import qualified Storage.Beam.RiderConfig as RC

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.System.QueryData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery _ _ req = do
  let tableName = req.tableName
  setClause' <- getKeyNameAndValue req.setClause
  case req.queryType of
    API.Types.RiderPlatform.Management.System.UPDATE -> do
      whereClause' <- getKeyNameAndValue req.whereClause
      _ <- queryTypeCheck tableName whereClause' setClause'
      executeQuery $ generateUpdateQuery tableName setClause' whereClause'
    API.Types.RiderPlatform.Management.System.INSERT -> do
      _ <- queryTypeCheck tableName [] setClause'
      executeQuery $ generateInsertQuery tableName setClause'

executeQuery :: Maybe Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
executeQuery query' = do
  case query' of
    Just q -> do
      conn <- asks (.psqlConn)
      res <- L.runIO $ try $ PG.execute_ conn (Query (TE.encodeUtf8 q))
      case res of
        Left (e :: SomeException) -> throwError $ InvalidRequest $ "Query execution failed: " <> Text.pack (show e)
        Right _ -> return Kernel.Types.APISuccess.Success
    Nothing -> throwError $ InvalidRequest "Invalid Query"

valueToSQL :: Value -> Text
valueToSQL (String s) = "'" <> Text.replace "'" "''" s <> "'" -- Quote and escape single quotes
valueToSQL (Number n) = case S.floatingOrInteger n of
  Left r -> Text.pack (show (r :: Double)) -- Floating point number
  Right i -> Text.pack (show (i :: Integer)) -- Integer number
valueToSQL (Bool b) = if b then "true" else "false" -- SQL boolean values
valueToSQL Null = "null" -- SQL NULL
valueToSQL (Object obj) = "'" <> TE.decodeUtf8 (BSL.toStrict (encode (Object obj))) <> "'::json"
valueToSQL (Array arr) = quote $ "{" <> Text.intercalate "," (map valueToSQL' (DV.toList arr)) <> "}"
  where
    valueToSQL' :: Value -> Text
    valueToSQL' (String s) = "'" <> Text.replace "'" "''" s <> "'"
    valueToSQL' (Number n) = case S.floatingOrInteger n of
      Left r -> Text.pack (show (r :: Double))
      Right i -> Text.pack (show (i :: Integer))
    valueToSQL' (Bool b) = if b then "true" else "false"
    valueToSQL' Null = "null"
    valueToSQL' (Object obj) = "'" <> TE.decodeUtf8 (BSL.toStrict (encode (Object obj))) <> "'::json"
    valueToSQL' (Array a) = "{" <> Text.intercalate "," (map valueToSQL' (DV.toList a)) <> "}"

quote :: Text -> Text
quote t = "'" <> Text.replace "'" "''" t <> "'"

generateInsertQuery :: Text -> [(Text, Value)] -> Maybe Text
generateInsertQuery tableName setClause =
  Just $ "INSERT INTO " <> tableName <> " (" <> Text.intercalate " , " (fst <$> setClause) <> ") VALUES (" <> Text.intercalate " , " (valueToSQL . snd <$> setClause) <> ");"

generateUpdateQuery :: Text -> [(Text, Value)] -> [(Text, Value)] -> Maybe Text
generateUpdateQuery tableName setClause whereClause = do
  let correctWhereClauseText = makeWhereCluase
      setQuery = makeSetClause
  if Text.null correctWhereClauseText
    then Nothing
    else Just $ "UPDATE " <> tableName <> " SET " <> setQuery <> " WHERE " <> correctWhereClauseText <> ";"
  where
    makeSetClause :: Text
    makeSetClause = do
      Text.intercalate " , " $ map (\(key, val') -> key <> " = " <> valueToSQL val') setClause
    makeWhereCluase :: Text
    makeWhereCluase = do
      Text.intercalate " AND " $ map (\(key, val') -> key <> " = " <> valueToSQL val') whereClause

getKeyNameAndValue :: (MonadThrow m, Log m) => Value -> m [(Text, Value)]
getKeyNameAndValue (Object obj) = return $ DTX.first DAK.toText <$> DAKM.toList obj
getKeyNameAndValue _ = throwError $ InvalidRequest "This should have been an object"

queryTypeCheck :: (MonadThrow m, Log m) => Text -> [(Text, Value)] -> [(Text, Value)] -> m ()
queryTypeCheck tableName' whereClause' setClause' = do
  let tableName = KP.last $ Text.splitOn "." tableName'
  mapM_
    ( \(tableColumn, value) -> do
        res <- bool (checkParseField tableName tableColumn value) (return True) (tableName' == "atlas_app.system_configs")
        if res then return True else throwError $ InvalidRequest ("Type Validation Failed For the column: " <> tableColumn)
    )
    whereClause'

  mapM_
    ( \(tableColumn, value) -> do
        res <- checkParseField tableName tableColumn value
        if res then return True else throwError $ InvalidRequest ("Type Validation Failed For the column: " <> tableColumn)
    )
    setClause'

snakeToCamel :: Text -> Text
snakeToCamel input = do
  let parts = Text.splitOn "_" input
   in case parts of
        [] -> ""
        (x : xs) -> Text.pack $ Text.unpack x ++ concatMap capitalize xs
  where
    capitalize txt =
      case Text.unpack txt of
        [] -> []
        (y : ys) -> DC.toUpper y : ys

checkParseField :: (MonadThrow m, Log m) => Text -> Text -> Value -> m Bool
checkParseField tableName tableColumn' value = do
  let tableColumn = snakeToCamel tableColumn'
  case tableName of
    "hot_spot_config" -> return $ checkParse (Proxy @HSC.HotSpotConfigT) tableColumn value
    "issue_config" -> return $ checkParse (Proxy @ISU.IssueT) tableColumn value
    "merchant_config" -> return $ checkParse (Proxy @MC.MerchantConfigT) tableColumn value
    "merchant_service_config" -> return $ checkParse (Proxy @MSC.MerchantServiceConfigT) tableColumn value
    "merchant_service_usage_config" -> return $ checkParse (Proxy @MSUC.MerchantServiceUsageConfigT) tableColumn value
    "payout_config" -> return $ checkParse (Proxy @PC.PayoutConfigT) tableColumn value
    "place_based_service_config" -> return $ checkParse (Proxy @PBSC.PlaceBasedServiceConfigT) tableColumn value
    "rider_config" -> return $ checkParse (Proxy @RC.RiderConfigT) tableColumn value
    "ride_related_notification_config" -> return $ checkParse (Proxy @RRNC.RideRelatedNotificationConfigT) tableColumn value
    "system_configs" -> return $ do
      case value of
        String value' -> isJust $ decodeFromText @Tables value'
        _ -> False
    _ -> throwError $ InvalidRequest $ "The table `" <> tableName <> "` is not supported. "
