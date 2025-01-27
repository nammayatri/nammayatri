module Domain.Action.Dashboard.Management.System (postSystemRunQuery, generateInsertQuery) where

import qualified API.Types.ProviderPlatform.Management.System
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
import Storage.Beam.DriverIntelligentPoolConfig as DIPC
import Storage.Beam.DriverPoolConfig as DPC
import qualified Storage.Beam.FarePolicy as FP
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as DEFB
import qualified Storage.Beam.FarePolicy.FarePolicyAmbulanceDetailsSlab as FPAD
import qualified Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as FPICDPS
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as FPFB
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as FPPDPEKRS
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails as FPRD
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as FPRDDB
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as FPRDPS
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as FPSS
import Storage.Beam.GoHomeConfig as GHC
import qualified Storage.Beam.MerchantOperatingCity as MOC
import qualified Storage.Beam.MerchantServiceConfig as MSC
import qualified Storage.Beam.MerchantServiceUsageConfig as MSUC
import qualified Storage.Beam.Overlay as OV
import qualified Storage.Beam.SurgePricing as SP
import Storage.Beam.TransporterConfig as MTC

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.System.QueryData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery _ _ req = do
  let tableName = req.tableName
  setClause' <- getKeyNameAndValue req.setClause
  case req.queryType of
    API.Types.ProviderPlatform.Management.System.UPDATE -> do
      whereClause' <- getKeyNameAndValue req.whereClause
      _ <- queryTypeCheck tableName whereClause' setClause'
      executeQuery $ generateUpdateQuery tableName setClause' whereClause'
    API.Types.ProviderPlatform.Management.System.INSERT -> do
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
    valueToSQL' Null = "null  "
    valueToSQL' (Object obj) = "'" <> TE.decodeUtf8 (BSL.toStrict (encode (Object obj))) <> "'::json"
    valueToSQL' (Array a) = "{" <> Text.intercalate "," (map valueToSQL' (DV.toList a)) <> "}"

quote :: Text -> Text
quote t = "'" <> Text.replace "'" "''" t <> "'"

generateInsertQuery :: Text -> [(Text, Value)] -> Maybe Text
generateInsertQuery tableName setClause =
  Just $ "INSERT INTO " <> tableName <> " (" <> Text.intercalate " , " (fst <$> setClause) <> ") VALUES (" <> Text.intercalate " , " ((valueToSQL .snd) <$> setClause) <> ");"

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

queryTypeCheck :: (MonadThrow m, Log m) => Text -> [(Text, Value)] -> [(Text, Value)] -> m ()
queryTypeCheck tableName' whereClause' setClause' = do
  let tableName = KP.last $ Text.splitOn "." tableName'
  mapM_
    ( \(tableColumn, value) -> do
        res <- bool (checkParseField tableName tableColumn value) (return True) (tableName' == "atlas_driver_offer_bpp.system_configs")
        if res then return True else throwError $ InvalidRequest ("Type Validation Failed For the column: " <> tableColumn)
    )
    whereClause'
  mapM_
    ( \(tableColumn, value) -> do
        res <- checkParseField tableName tableColumn value
        if res then return True else throwError $ InvalidRequest ("Type Validation Failed For the column: " <> tableColumn)
    )
    setClause'

getKeyNameAndValue :: (MonadThrow m, Log m) => Value -> m [(Text, Value)]
getKeyNameAndValue obj = do
  case obj of
    Object km -> return $ DTX.first DAK.toText <$> DAKM.toList km
    _ -> throwError $ InvalidRequest "This should have been an object"

snakeToCamel :: Text -> Text
snakeToCamel input =
  let parts = Text.splitOn "_" input
   in case parts of
        [] -> ""
        (x : xs) -> Text.pack $ Text.unpack x ++ concatMap capitalize xs
  where
    capitalize txt = case Text.unpack txt of
      [] -> []
      (y : ys) -> DC.toUpper y : ys

checkParseField :: (MonadThrow m, Log m) => Text -> Text -> Value -> m Bool
checkParseField tableName tableColumn' value = do
  let tableColumn = snakeToCamel tableColumn'
  case tableName of
    "driver_pool_config" -> return $ checkParse (Proxy @DPC.DriverPoolConfigT) tableColumn value
    "driver_intelligent_pool_config" -> return $ checkParse (Proxy @DIPC.DriverIntelligentPoolConfigT) tableColumn value
    "transporter_config" -> return $ checkParse (Proxy @MTC.TransporterConfigT) tableColumn value
    "go_home_config" -> return $ checkParse (Proxy @GHC.GoHomeConfigT) tableColumn value
    "fare_policy_progressive_details_per_extra_km_rate_section" -> return $ checkParse (Proxy @[FPPDPEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection]) tableColumn value
    "fare_policy_rental_details_distance_buffers" -> return $ checkParse (Proxy @[FPRDDB.FarePolicyRentalDetailsDistanceBuffers]) tableColumn value
    "fare_policy_rental_details_pricing_slabs" -> return $ checkParse (Proxy @[FPRDPS.FarePolicyRentalDetailsPricingSlabs]) tableColumn value
    "fare_policy_inter_city_details_pricing_slabs" -> return $ checkParse (Proxy @[FPICDPS.FarePolicyInterCityDetailsPricingSlabs]) tableColumn value
    "fare_policy_slabs_details_slab" -> return $ checkParse (Proxy @[FPSS.FarePolicySlabsDetailsSlab]) tableColumn value
    "driver_extra_fee_bounds" -> return $ checkParse (Proxy @DEFB.DriverExtraFeeBoundsT) tableColumn value
    "fare_policy_progressive_details" -> return $ checkParse (Proxy @FPFB.FarePolicyProgressiveDetailsT) tableColumn value
    "fare_policy_rental_details" -> return $ checkParse (Proxy @FPRD.FarePolicyRentalDetailsT) tableColumn value
    "fare_policy" -> return $ checkParse (Proxy @FP.FarePolicyT) tableColumn value
    "fare_policy_ambulance_details" -> return $ checkParse (Proxy @FPAD.FarePolicyAmbulanceDetailsSlabT) tableColumn value
    "merchant_operating_city" -> return $ checkParse (Proxy @MOC.MerchantOperatingCityT) tableColumn value
    "merchant_service_config" -> return $ checkParse (Proxy @MSC.MerchantServiceConfigT) tableColumn value
    "merchant_service_usage_config" -> return $ checkParse (Proxy @MSUC.MerchantServiceUsageConfigT) tableColumn value
    "overlay" -> return $ checkParse (Proxy @OV.OverlayT) tableColumn value
    "surge_pricing" -> return $ checkParse (Proxy @SP.SurgePricingT) tableColumn value
    "system_configs" -> return $ do
      case value of
        String value' -> isJust $ decodeFromText @Tables value'
        _ -> False
    _ -> throwError $ InvalidRequest $ "The table `" <> tableName <> "` is not supported. "
