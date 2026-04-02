module Domain.Action.Internal.SettlementReportIngestion where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig as DMSC
import Environment
import Kernel.External.Settlement.Types (SettlementService (..), SettlementServiceConfig (..), SettlementSourceConfig)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Settlement.Ingestion (ingestPaymentSettlementReport)
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

data TriggerSettlementReportIngestionReq = TriggerSettlementReportIngestionReq
  { merchantId :: Id DM.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data ServiceIngestionResult = ServiceIngestionResult
  { service :: Text,
    totalParsed :: Int,
    totalStored :: Int,
    totalDuplicates :: Int,
    totalFailed :: Int,
    parseErrors :: [Text],
    storeErrors :: [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data TriggerSettlementReportIngestionRes = TriggerSettlementReportIngestionRes
  { message :: Text,
    configsFound :: Int,
    results :: [ServiceIngestionResult]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

triggerSettlementReportIngestion :: TriggerSettlementReportIngestionReq -> Flow TriggerSettlementReportIngestionRes
triggerSettlementReportIngestion req = do
  let merchantId = req.merchantId
      merchantOperatingCityId = req.merchantOperatingCityId

  logInfo $ "Triggering SettlementReportIngestion for merchant: " <> merchantId.getId <> ", city: " <> merchantOperatingCityId.getId

  settlementConfigs <- getSettlementConfigs merchantOperatingCityId
  logInfo $ "Found " <> show (length settlementConfigs) <> " settlement service configs"

  if null settlementConfigs
    then do
      logWarning "No SettlementService configs found in MerchantServiceConfig"
      pure $
        TriggerSettlementReportIngestionRes
          { message = "No SettlementService configs found in MerchantServiceConfig",
            configsFound = 0,
            results = []
          }
    else do
      serviceResults <- forM settlementConfigs $ \(service, sourceConfig) -> do
        logInfo $ "Processing settlement service: " <> show service
        result <- ingestPaymentSettlementReport sourceConfig service merchantId.getId merchantOperatingCityId.getId
        logInfo $
          "Ingestion result for " <> show service
            <> " - parsed: " <> show result.totalParsed
            <> ", stored: " <> show result.totalStored
            <> ", duplicates: " <> show result.totalDuplicates
            <> ", failed: " <> show result.totalFailed
        when (result.totalFailed > 0) $
          logError $
            "Settlement ingestion for " <> show service <> " had " <> show result.totalFailed
              <> " failures out of "
              <> show result.totalParsed
              <> " rows"
        when (not $ null result.parseErrors) $
          logError $ "Parse errors for " <> show service <> ": " <> show result.parseErrors
        when (not $ null result.storeErrors) $
          logError $ "Store errors for " <> show service <> ": " <> show result.storeErrors
        pure $
          ServiceIngestionResult
            { service = show service,
              totalParsed = result.totalParsed,
              totalStored = result.totalStored,
              totalDuplicates = result.totalDuplicates,
              totalFailed = result.totalFailed,
              parseErrors = result.parseErrors,
              storeErrors = result.storeErrors
            }

      let totalStored = sum $ map (.totalStored) serviceResults
          totalFailed = sum $ map (.totalFailed) serviceResults
      logInfo $ "Settlement ingestion complete - total stored: " <> show totalStored <> ", total failed: " <> show totalFailed

      pure $
        TriggerSettlementReportIngestionRes
          { message = "Settlement ingestion complete. Stored: " <> show totalStored <> ", Failed: " <> show totalFailed,
            configsFound = length settlementConfigs,
            results = serviceResults
          }

getSettlementConfigs ::
  Id DMOC.MerchantOperatingCity ->
  Flow [(SettlementService, SettlementSourceConfig)]
getSettlementConfigs mOpCityId = do
  let allSettlementServices = [minBound .. maxBound] :: [SettlementService]
  configs <- forM allSettlementServices $ \service -> do
    mbConfig <- CQMSC.findByServiceAndCity (DMSC.SettlementService service) mOpCityId
    logInfo $ "Checking config for service " <> show service <> ": " <> if isJust mbConfig then "FOUND" else "NOT FOUND"
    pure $ case mbConfig of
      Just cfg -> case cfg.serviceConfig of
        DMSC.SettlementServiceConfig settlementCfg -> case settlementCfg of
          HyperPGConfig srcCfg -> Just (service, srcCfg)
          BillDeskConfig srcCfg -> Just (service, srcCfg)
        _ -> Nothing
      Nothing -> Nothing
  pure $ catMaybes configs
