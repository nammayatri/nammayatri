{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.System (postSystemRunQuery) where

import qualified API.Types.ProviderPlatform.Management.System
import Data.Aeson
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.ByteString.Lazy as BSL
import Data.Char as DC
import Data.OpenApi (ToSchema)
import Data.Pool (Pool, withResource)
import qualified Data.Scientific as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Text.Format (format)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Tuple.Extra as DTX
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.Types (Query (Query))
import qualified Domain.Types.Merchant
import Domain.Types.UtilsTH
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as KP
import Kernel.Storage.Esqueleto hiding (Value)
import Kernel.Storage.Esqueleto.Config (EsqDBConfig)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.Error.Throwing (throwError)
import Kernel.Utils.Logging (logDebug)
import Servant hiding (throwError)
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
import Storage.Beam.TransporterConfig as MTC
import Tools.Auth

toConnectInfo :: EsqDBConfig -> ConnectInfo
toConnectInfo config =
  ConnectInfo
    { connectHost = Text.unpack config.connectHost,
      connectPort = config.connectPort,
      connectUser = Text.unpack config.connectUser,
      connectPassword = Text.unpack config.connectPassword,
      connectDatabase = Text.unpack config.connectDatabase
    }

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.System.QueryData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery _ _ req = do
  let tableName = req.tableName
  whereClause' <- getKeyNameAndValue req.whereClause
  setClause' <- getKeyNameAndValue req.setClause
  _ <- queryTypeCheck tableName whereClause' setClause'
  let query' = generateUpdateQuery tableName setClause' whereClause'
  case query' of
    Just q -> do
      -- executeQuery conn q
      config <- asks (.esqDBCfg)
      res <- L.runIO $ try $ PG.withConnect (toConnectInfo config) (\conn -> PG.execute_ conn (Query (TE.encodeUtf8 q)))
      -- res <- L.runIO $ try $ PG.execute_ conn (Query (TE.encodeUtf8 q))
      case res of
        Left (e :: SomeException) -> throwError $ InvalidRequest $ "Query execution failed: " <> Text.pack (show e)
        Right _ -> return Kernel.Types.APISuccess.Success
    Nothing -> throwError $ InvalidRequest "Invalid Query"

valueToSQL :: Value -> Text
valueToSQL (String s) = "'" <> Text.replace "'" "''" s <> "'" -- Quote and escape single quotes
valueToSQL (Number n) = case S.floatingOrInteger n of
  Left r -> Text.pack (show (r :: Double)) -- Floating point number
  Right i -> Text.pack (show (i :: Integer)) -- Integer number
valueToSQL (Bool b) = if b then "TRUE" else "FALSE" -- SQL boolean values
valueToSQL Null = "NULL" -- SQL NULL
valueToSQL (Object obj) = "'" <> TE.decodeUtf8 (BSL.toStrict (encode (Object obj))) <> "'::json"
valueToSQL (Array arr) = "'" <> TE.decodeUtf8 (BSL.toStrict (encode (Array arr))) <> "'::json"

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
        res <- checkParseField tableName tableColumn value
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

checkParseField :: (Monad m, Log m) => Text -> Text -> Value -> m Bool
checkParseField tableName tableColumn' value = do
  let tableColumn = snakeToCamel tableColumn'
  return case tableName of
    "driver_pool_config" -> checkParse (Proxy @DPC.DriverPoolConfigT) tableColumn value
    "driver_intelligent_pool_config" -> checkParse (Proxy @DIPC.DriverIntelligentPoolConfigT) tableColumn value
    "transporter_config" -> checkParse (Proxy @MTC.TransporterConfigT) tableColumn value
    "go_home_config" -> checkParse (Proxy @GHC.GoHomeConfigT) tableColumn value
    "fare_policy_progressive_details_per_extra_km_rate_section" -> checkParse (Proxy @[FPPDPEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection]) tableColumn value
    "fare_policy_rental_details_distance_buffers" -> checkParse (Proxy @[FPRDDB.FarePolicyRentalDetailsDistanceBuffers]) tableColumn value
    "fare_policy_rental_details_pricing_slabs" -> checkParse (Proxy @[FPRDPS.FarePolicyRentalDetailsPricingSlabs]) tableColumn value
    "fare_policy_inter_city_details_pricing_slabs" -> checkParse (Proxy @[FPICDPS.FarePolicyInterCityDetailsPricingSlabs]) tableColumn value
    "fare_policy_slabs_details_slab" -> checkParse (Proxy @[FPSS.FarePolicySlabsDetailsSlab]) tableColumn value
    "driver_extra_fee_bounds" -> checkParse (Proxy @DEFB.DriverExtraFeeBoundsT) tableColumn value
    "fare_policy_progressive_details" -> checkParse (Proxy @FPFB.FarePolicyProgressiveDetailsT) tableColumn value
    "fare_policy_rental_details" -> checkParse (Proxy @FPRD.FarePolicyRentalDetailsT) tableColumn value
    "fare_policy" -> checkParse (Proxy @FP.FarePolicyT) tableColumn value
    "fare_policy_ambulance_details" -> checkParse (Proxy @FPAD.FarePolicyAmbulanceDetailsSlabT) tableColumn value
    _ -> True
