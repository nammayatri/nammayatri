{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.Management.NammaTag
  ( postNammaTagTagCreate,
    postNammaTagTagVerify,
    postNammaTagTagUpdate,
    deleteNammaTagTagDelete,
    getNammaTagTagAll,
    getNammaTagTagDetails,
    getNammaTagQueryDetails,
    postNammaTagQueryCreate,
    postNammaTagQueryUpdate,
    deleteNammaTagQueryDelete,
    postNammaTagAppDynamicLogicVerify,
    getNammaTagAppDynamicLogic,
    postNammaTagRunJob,
    postNammaTagTimeBoundsCreate,
    deleteNammaTagTimeBoundsDelete,
    getNammaTagAppDynamicLogicGetLogicRollout,
    postNammaTagAppDynamicLogicUpsertLogicRollout,
    getNammaTagTimeBounds,
    getNammaTagAppDynamicLogicVersions,
    getNammaTagAppDynamicLogicDomains,
    getNammaTagAppDynamicLogicDomainsAndEvents,
    getNammaTagAppDynamicLogicGetDomainSchema,
    getNammaTagQueryAll,
    postNammaTagConfigPilotGetVersion,
    postNammaTagConfigPilotGetConfig,
    postNammaTagConfigPilotCreateUiConfig,
    getNammaTagConfigPilotAllConfigs,
    getNammaTagConfigPilotConfigDetails,
    getNammaTagConfigPilotGetTableData,
    getNammaTagConfigPilotAlwaysOnList,
    postNammaTagConfigPilotActionChange,
    getNammaTagConfigPilotAllUiConfigs,
    getNammaTagConfigPilotUiConfigDetails,
    getNammaTagConfigPilotGetUiTableData,
    postNammaTagConfigPilotGetPatchedElement,
    postNammaTagConfigPilotGetConfigWithDimensions,
    getNammaTagConfigPilotGetDimensionSchema,
    postNammaTagConfigPilotCreateRow,
    getNammaTagBehaviorVisibility,
  )
where

import qualified ConfigPilotFrontend.Flow as CPF
import qualified ConfigPilotFrontend.Types as CPT
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Singletons
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), fromGregorian)
import qualified Domain.Types.Coins.CoinsConfig as DCC
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DriverPoolConfig as DTD
import qualified Domain.Types.Exophone as DTEXO
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as DFODVC
import qualified Domain.Types.GoHomeConfig as DGHC
import qualified "beckn-spec" Domain.Types.Invoice as DTI
import qualified Domain.Types.LeaderBoardConfigs as DLBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantMessage as DTM
import qualified Domain.Types.MerchantPushNotification as DTPN
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.Overlay as DTOVL
import qualified Domain.Types.PayoutConfig as DTP
import qualified Domain.Types.ReminderConfig as DRMC
import qualified Domain.Types.RideRelatedNotificationConfig as DTRN
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.Translations as DTTR
import qualified Domain.Types.TransporterConfig as DTT
import Domain.Types.UiDriverConfig (UiDriverConfig (..))
import qualified Domain.Types.UiDriverConfig as DTDC
import qualified Domain.Types.Yudhishthira
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified "shared-services" IssueManagement.Domain.Types.Issue.IssueConfig as DIC
import qualified "shared-services" IssueManagement.Storage.Queries.Issue.IssueConfig as SQICfg
import Kernel.External.Types (Language (ENGLISH))
import qualified Kernel.Prelude as Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.BehaviorEngine.Types as BET
import qualified Lib.BehaviorTracker.Types as BTT
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Finance.Invoice.RenderTemplate as FRT
import qualified Lib.Scheduler.JobStorageType.DB.Queries as QDBJ
import Lib.Scheduler.Types (AnyJob (..))
import qualified Lib.Yudhishthira.Flow.Dashboard as YudhishthiraFlow
import qualified Lib.Yudhishthira.SchemaInstances ()
import Lib.Yudhishthira.SchemaTH
import Lib.Yudhishthira.SchemaUtils
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Storage.Queries.NammaTagTriggerV2 as QNammaTagTriggerV2
import qualified Lib.Yudhishthira.Storage.Queries.NammaTagV2 as QNammaTagV2
import qualified Lib.Yudhishthira.Storage.Queries.TagActionNotificationConfig as SQTANC
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.Common as C
import qualified Lib.Yudhishthira.Types.NammaTagV2
import qualified Lib.Yudhishthira.Types.TagActionNotificationConfig as DTANC
import qualified Lib.Yudhishthira.TypesTH as YTH
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.BehaviourManagement.Visibility as BehaviorVisibility
import SharedLogic.CancellationCoins
import SharedLogic.DriverPool.Config (Config (..))
import SharedLogic.DriverPool.Types
import SharedLogic.DynamicPricing
import qualified SharedLogic.KaalChakra.Chakras as Chakras
import SharedLogic.Merchant
import SharedLogic.UserCancellationDues
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.UiDriverConfig as QUiConfig
import Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..))
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.DriverPoolConfig (DriverPoolConfigDimensions (..))
import Storage.ConfigPilot.Config.Exophone (ExophoneDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.GoHomeConfig (GoHomeConfigDimensions (..))
import Storage.ConfigPilot.Config.IssueConfig (IssueConfigDimensions (..))
import Storage.ConfigPilot.Config.LeaderBoardConfigs (LeaderBoardConfigsDimensions (..))
import Storage.ConfigPilot.Config.MerchantMessage (MerchantMessageDimensions (..))
import Storage.ConfigPilot.Config.MerchantPushNotification (MerchantPushNotificationDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.MerchantServiceUsageConfig (MerchantServiceUsageConfigDimensions (..))
import Storage.ConfigPilot.Config.Overlay (OverlayDimensions (..))
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.ReminderConfig (ReminderConfigDimensions (..))
import Storage.ConfigPilot.Config.RideRelatedNotificationConfig (RideRelatedNotificationConfigDimensions (..))
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.TagActionNotificationConfig (TagActionNotificationConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Storage.ConfigPilot.Config.UiDriverConfig (UiDriverConfigDimensions (..))
import qualified Storage.Queries.Coins.CoinsConfig as SQCCfg
import qualified Storage.Queries.DocumentVerificationConfig as SQDVC
import qualified Storage.Queries.DriverPoolConfig as SQDPC
import qualified Storage.Queries.Exophone as SQEXO
import qualified Storage.Queries.FleetOwnerDocumentVerificationConfig as SQFODVC
import qualified Storage.Queries.GoHomeConfig as SQGHC
import qualified Storage.Queries.LeaderBoardConfigs as SQLBC
import qualified Storage.Queries.MerchantMessage as SQMM
import qualified Storage.Queries.MerchantPushNotification as SQMPN
import qualified Storage.Queries.MerchantServiceConfigExtra as SQMSCE
import qualified Storage.Queries.MerchantServiceUsageConfig as SQMSUC
import qualified Storage.Queries.Overlay as SQOVL
import qualified Storage.Queries.PayoutConfig as SQPC
import qualified Storage.Queries.ReminderConfig as SQRMC
import qualified Storage.Queries.RideRelatedNotificationConfig as SQRRNC
import qualified Storage.Queries.ScheduledPayoutConfig as SQSPC
import qualified Storage.Queries.Translations as SQTR
import qualified Storage.Queries.TranslationsExtra as SQTRE
import qualified Storage.Queries.TransporterConfig as SQTC
import qualified Storage.Queries.UiDriverConfig as SQU
import qualified Tools.ConfigPilot as TC
import qualified Tools.DynamicLogic as TDL

$(YTH.generateGenericDefault ''DTP.PayoutConfig)
$(YTH.generateGenericDefault ''DTRN.RideRelatedNotificationConfig)
$(YTH.generateGenericDefault ''DTT.TransporterConfig) -- TODO ERROR
$(YTH.generateGenericDefault ''DTM.MerchantMessage) -- TODO ERROR
$(YTH.generateGenericDefault ''DTPN.MerchantPushNotification)

$(YTH.generateGenericDefault ''DTD.DriverPoolConfig)
$(YTH.generateGenericDefault ''DMSUC.MerchantServiceUsageConfig)
$(YTH.generateGenericDefault ''DDVC.DocumentVerificationConfig)
$(YTH.generateGenericDefault ''DGHC.GoHomeConfig)
$(YTH.generateGenericDefault ''DLBC.LeaderBoardConfigs)
$(YTH.generateGenericDefault ''DRMC.ReminderConfig)
$(YTH.generateGenericDefault ''DSPC.ScheduledPayoutConfig)
$(YTH.generateGenericDefault ''DTANC.TagActionNotificationConfig)
$(YTH.generateGenericDefault ''DFODVC.FleetOwnerDocumentVerificationConfig)
$(YTH.generateGenericDefault ''DCC.CoinsConfig)

$(genToSchema ''DTP.PayoutConfig)
$(genToSchema ''DTRN.RideRelatedNotificationConfig)
$(genToSchema ''DTT.TransporterConfig)
$(genToSchema ''DTPN.MerchantPushNotification)
$(genToSchema ''DTD.DriverPoolConfig)
$(genToSchema ''DMSUC.MerchantServiceUsageConfig)
$(genToSchema ''DDVC.DocumentVerificationConfig)
$(genToSchema ''DGHC.GoHomeConfig)
$(genToSchema ''DLBC.LeaderBoardConfigs)
$(genToSchema ''DRMC.ReminderConfig)
$(genToSchema ''DSPC.ScheduledPayoutConfig)
$(genToSchema ''DTANC.TagActionNotificationConfig)
$(genToSchema ''DFODVC.FleetOwnerDocumentVerificationConfig)
$(genToSchema ''DCC.CoinsConfig)
$(genToSchema ''MerchantServiceConfigDimensions)
$(genToSchema ''TaggedDriverPoolInput)
$(genToSchema ''CancellationCoinData)
$(genToSchema ''DynamicPricingData)
$(genToSchema ''UserCancellationDuesData)
$(genToSchema ''UserCancellationDuesWaiveOffData)
$(genToSchema ''FRT.InvoiceContext)

instance Default FRT.InvoiceContext where
  def =
    FRT.InvoiceContext
      { invoiceNumber = "",
        referenceInvoiceNumber = Nothing,
        issuedAt = UTCTime (fromGregorian 1970 1 1) 0,
        dueAt = Nothing,
        invoiceType = DTI.Ride,
        language = ENGLISH,
        currency = INR,
        currencyCode = "",
        merchantId = "",
        merchantShortId = "",
        paymentMode = Nothing,
        hasAdjustments = False,
        issuedToName = Nothing,
        issuedToAddress = Nothing,
        supplierName = Nothing,
        supplierAddress = Nothing,
        supplierGSTIN = Nothing,
        supplierTaxNo = Nothing,
        issuedByName = Nothing,
        issuedByAddress = Nothing,
        merchantGstin = Nothing,
        sellerTradeName = Nothing,
        mbRecipientBusinessId = Nothing,
        mbSellerBusinessId = Nothing,
        mbSellerVatNumber = Nothing,
        logoUrl = Nothing,
        appName = Nothing,
        periodStart = Nothing,
        periodEnd = Nothing,
        taxTxnRate = Nothing,
        taxTxnGstRate = Nothing,
        cardBrand = Nothing,
        cardLastFour = Nothing,
        lineItems = []
      }

instance Default FRT.RenderLineItem where
  def =
    FRT.RenderLineItem
      { description = "",
        descriptionType = Nothing,
        amount = 0,
        taxAmount = 0,
        groupId = Nothing,
        isExternalCharge = False,
        itemType = Nothing,
        language = ENGLISH
      }

postNammaTagTagCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateNammaTagRequest -> Environment.Flow LYT.CreateNammaTagResponse)
postNammaTagTagCreate merchantShortId opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  defaultRunResult <- runAgainstDefault
  void $ YudhishthiraFlow.postTagCreate (cast merchantOperatingCity.id) req
  pure defaultRunResult
  where
    runAgainstDefault = do
      case req of
        LYT.ApplicationTag LYT.NammaTagApplication {..} -> do
          case tagRule of
            C.RuleEngine rule -> do
              unless (length tagStages == length (NE.nub tagStages)) $
                throwError (InvalidRequest "Tag stages should be unique")
              createTagResponses <- forM tagStages \event -> do
                let defVal = C.getLogicInputDef event
                case defVal of
                  Nothing -> throwError $ InvalidRequest "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
                  Just defaultVal -> do
                    result <- YudhishthiraFlow.verifyDynamicLogic tagPossibleValues [rule] defaultVal
                    pure $ LYT.ApplicationTagRes (LYT.CreateNammaApplicationTagResponse result defaultVal)
              pure $ LYT.CreateNammaTagResponse {results = createTagResponses}
            _ -> throwError $ InvalidRequest "LLMContext not supported yet"
        _ -> pure $ LYT.CreateNammaTagResponse {results = NE.singleton LYT.Success}

postNammaTagTagUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UpdateNammaTagRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagTagUpdate merchantShortId opCity req = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  YudhishthiraFlow.postTagUpdate (cast merchantOperatingCity.id) req

postNammaTagTagVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.VerifyNammaTagRequest -> Environment.Flow LYT.VerifyNammaTagResponse)
postNammaTagTagVerify merchantShortId opCity LYT.VerifyNammaTagRequest {..} = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let merchantOpCityId = cast merchantOperatingCity.id
  case source of
    LYT.Application tagStage -> do
      let val =
            if useDefaultData
              then C.getLogicInputDef tagStage
              else logicData <|> C.getLogicInputDef tagStage
      case val of
        Just value -> do
          -- validating data provided gets parsed to Stage InputData type.
          validateInputType tagStage value
          result <- YudhishthiraFlow.verifyEventLogic merchantOpCityId tagStage [logic] value
          pure $ LYT.VerifyNammaTagResponse {executionResult = result, dataUsed = value}
        Nothing -> throwError $ InvalidRequest "No data supplied and failed to get default for the specified event, check if `getLogicInputDef` is defined for your event in `instance YTC.LogicInputLink YA.ApplicationEvent`"
    _ -> do
      throwError $ InvalidRequest $ "Available only for Application events currenlty"
  where
    validateInputType tagStage value =
      case tagStage of
        LYT.Search -> do
          _ :: Domain.Types.Yudhishthira.TagData <- parseOrThrowError value
          pure ()
        LYT.RideEnd -> do
          _ :: Domain.Types.Yudhishthira.EndRideTagData <- parseOrThrowError value
          pure ()
        LYT.RideCancel -> do
          _ :: Domain.Types.Yudhishthira.CancelRideTagData <- parseOrThrowError value
          pure ()
        _ -> throwError $ InvalidRequest $ "Only supported for Search, Cancel and EndRide event for now"

    parseOrThrowError value =
      case A.fromJSON value of
        A.Success res -> pure res
        A.Error err -> throwError $ InvalidRequest $ show err

deleteNammaTagTagDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteNammaTagTagDelete merchantShortId opCity tagName = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  YudhishthiraFlow.deleteTag (cast merchantOperatingCity.id) tagName

getNammaTagTagAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow [LYT.NammaTagDetailsResp]
getNammaTagTagAll merchantShortId opCity = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let mocId = cast merchantOperatingCity.id
  tags <- QNammaTagV2.findAllByMerchantOperatingCityId mocId
  allTriggers <- QNammaTagTriggerV2.findAllByMerchantOperatingCityId mocId
  let triggerMap = Map.fromListWith (++) [(t.tagName, [t.event]) | t <- allTriggers]
  return $ map (\tag -> mkNammaTagDetailsResp tag (fromMaybe [] (Map.lookup tag.name triggerMap)) Map.empty) tags

getNammaTagTagDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Text -> Environment.Flow LYT.NammaTagDetailsResp
getNammaTagTagDetails merchantShortId opCity tagName = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let mocId = cast merchantOperatingCity.id
  tag <- QNammaTagV2.findByPrimaryKey mocId tagName >>= fromMaybeM (InvalidRequest $ "NammaTag not found: " <> tagName)
  triggers <- QNammaTagTriggerV2.findAllByMerchantOperatingCityIdAndTagName mocId tagName
  let events = map (.event) triggers
      inputDataMap = Map.fromList $ map (\e -> (show e, C.getLogicInputDef e)) events
  return $ mkNammaTagDetailsResp tag events inputDataMap

-- ChakraQueries are global (not merchant-scoped), so merchant/city params are intentionally unused
getNammaTagQueryDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.Chakra -> Prelude.Text -> Environment.Flow LYT.ChakraQueriesAPIEntity
getNammaTagQueryDetails _merchantShortId _opCity chakra queryName = YudhishthiraFlow.getChakraQueryDetails chakra queryName

postNammaTagQueryCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueriesAPIEntity -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNammaTagQueryCreate _merchantShortId _opCity req = YudhishthiraFlow.postQueryCreate req

postNammaTagQueryUpdate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueryUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagQueryUpdate _merchantShortId _opCity = YudhishthiraFlow.postQueryUpdate

deleteNammaTagQueryDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ChakraQueryDeleteReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagQueryDelete _merchantShortId _opCity = YudhishthiraFlow.queryDelete

postNammaTagAppDynamicLogicVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.AppDynamicLogicReq -> Environment.Flow LYT.AppDynamicLogicResp)
postNammaTagAppDynamicLogicVerify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let mbMerchantId = Just $ cast merchant.id
  resp <- case req.domain of
    LYT.POOLING -> do
      driversData :: [DriverPoolWithActualDistResult] <- mapM (YudhishthiraFlow.createLogicData def . Just) req.inputData
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy TaggedDriverPoolInput) transporterConfig.referralLinkPassword req (TaggedDriverPoolInput driversData False 0)
    LYT.CANCELLATION_COIN_POLICY -> do
      logicData :: CancellationCoinData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy CancellationCoinResult) transporterConfig.referralLinkPassword req logicData
    LYT.DYNAMIC_PRICING_UNIFIED -> do
      logicData :: DynamicPricingData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy DynamicPricingResult) transporterConfig.referralLinkPassword req logicData
    LYT.USER_CANCELLATION_DUES -> do
      logicData :: UserCancellationDuesData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy UserCancellationDuesResult) transporterConfig.referralLinkPassword req logicData
    LYT.GPS_TOLL_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.CANCELLATION_RATE_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.ISSUE_BREACH_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.DRUNK_DRIVE_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.TOLL_ISSUE_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.AC_RESTRICTION_BEHAVIOR -> do
      logicData :: BTT.BehaviorSnapshot <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy BET.OrchestratedOutput) transporterConfig.referralLinkPassword req logicData
    LYT.USER_CANCELLATION_DUES_WAIVE_OFF -> do
      logicData :: UserCancellationDuesWaiveOffData <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy UserCancellationDuesWaiveOffResult) transporterConfig.referralLinkPassword req logicData
    LYT.CONFIG LYT.DriverPoolConfig -> do
      logicData :: Config <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy Config) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "DriverPoolConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTD.DriverPoolConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTD.DriverPoolConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTD.DriverPoolConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.UI_DRIVER dt pt -> do
      let uiConfigReq = LYT.UiConfigRequest {os = dt, platform = pt, merchantId = getId merchant.id, city = opCity, language = Nothing, bundle = Nothing, toss = Nothing}
      defaultConfig <- SQU.findUIConfig uiConfigReq merchantOpCityId >>= fromMaybeM (InvalidRequest "No default found for UiDriverConfig")
      let configWrap = LYT.Config defaultConfig.config Nothing 1
      logicData :: (LYT.Config Value) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      url <- TC.getTSServiceUrl
      YudhishthiraFlow.verifyAndUpdateUIDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config Value)) transporterConfig.referralLinkPassword req logicData url
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "PayoutConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTP.PayoutConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTP.PayoutConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "RideRelatedNotificationConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTRN.RideRelatedNotificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTRN.RideRelatedNotificationConfig)) transporterConfig.referralLinkPassword req logicData
    -- LYT.DRIVER_CONFIG LYT.MerchantMessage -> do
    --   defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTM.MerchantMessage)) >>= fromMaybeM (InvalidRequest "MerchantMessage config not found")
    --   let configWrap = LYT.Config defaultConfig Nothing 1
    --   logicData :: (LYT.Config DTM.MerchantMessage) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
    --   YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (Proxy :: Proxy DTM.MerchantMessage) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      defaultConfig <- fromMaybeM (InvalidRequest "MerchantPushNotification config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTPN.MerchantPushNotification) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTPN.MerchantPushNotification)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
      defaultConfig <- (pure $ Prelude.listToMaybe $ YTH.genDef (Proxy @DTT.TransporterConfig)) >>= fromMaybeM (InvalidRequest "Transporter config not found")
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTT.TransporterConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTT.TransporterConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.INVOICE_TEMPLATE _scope -> do
      logicData :: FRT.InvoiceContext <- YudhishthiraFlow.createLogicData def (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy A.Value) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.MerchantServiceUsageConfigDriver -> do
      defaultConfig <- fromMaybeM (InvalidRequest "MerchantServiceUsageConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DMSUC.MerchantServiceUsageConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DMSUC.MerchantServiceUsageConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DMSUC.MerchantServiceUsageConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "DocumentVerificationConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DDVC.DocumentVerificationConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DDVC.DocumentVerificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DDVC.DocumentVerificationConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.GoHomeConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "GoHomeConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DGHC.GoHomeConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DGHC.GoHomeConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DGHC.GoHomeConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.LeaderBoardConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "LeaderBoardConfigs config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DLBC.LeaderBoardConfigs))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DLBC.LeaderBoardConfigs) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DLBC.LeaderBoardConfigs)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.ReminderConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "ReminderConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DRMC.ReminderConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DRMC.ReminderConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DRMC.ReminderConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.ScheduledPayoutConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "ScheduledPayoutConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DSPC.ScheduledPayoutConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DSPC.ScheduledPayoutConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DSPC.ScheduledPayoutConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.TagActionNotificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "TagActionNotificationConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTANC.TagActionNotificationConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DTANC.TagActionNotificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DTANC.TagActionNotificationConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "FleetOwnerDocumentVerificationConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DFODVC.FleetOwnerDocumentVerificationConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DFODVC.FleetOwnerDocumentVerificationConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DFODVC.FleetOwnerDocumentVerificationConfig)) transporterConfig.referralLinkPassword req logicData
    LYT.DRIVER_CONFIG LYT.CoinsConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "CoinsConfig config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DCC.CoinsConfig))
      let configWrap = LYT.Config defaultConfig Nothing 1
      logicData :: (LYT.Config DCC.CoinsConfig) <- YudhishthiraFlow.createLogicData configWrap (Prelude.listToMaybe req.inputData)
      YudhishthiraFlow.verifyAndUpdateDynamicLogic mbMerchantId (cast merchantOpCityId) (Proxy :: Proxy (LYT.Config DCC.CoinsConfig)) transporterConfig.referralLinkPassword req logicData
    _ -> throwError $ InvalidRequest "Logic Domain not supported"

  when resp.isRuleUpdated $ case req.domain of
    LYT.DRIVER_CONFIG cfgType -> do
      TDL.deleteConfigHashKey (cast merchantOpCityId) req.domain
      invalidateConfigInMem (TC.toCacheConfigType cfgType)
      logDebug $ "CP Log: Cleared Cache for " <> show cfgType
    _ -> pure ()
  pure resp

getNammaTagAppDynamicLogic :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Int -> LYT.LogicDomain -> Environment.Flow [LYT.GetLogicsResp]
getNammaTagAppDynamicLogic merchantShortId opCity mbVersion domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getAppDynamicLogicForDomain (cast merchantOpCityId) mbVersion domain

postNammaTagRunJob ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  LYT.RunKaalChakraJobReq ->
  Environment.Flow LYT.RunKaalChakraJobRes
postNammaTagRunJob merchantShortId opCity req = do
  mbMerchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
  let mbMerchantOpCityId = mbMerchantOperatingCity <&> (.id)
  let mbMerchantId = mbMerchantOperatingCity <&> (.merchantId)
  -- Logic for complete old jobs works only for DbBased jobs
  whenJust req.completeOldJob $ \oldJobId -> do
    mbOldJob :: Maybe (AnyJob AllocatorJobType) <- QDBJ.findById oldJobId
    case mbOldJob of
      Nothing -> throwError (InvalidRequest "Job not found")
      Just (AnyJob oldJob) -> do
        let jobType = fromSing $ oldJob.jobInfo.jobType
        unless (castChakra jobType == Just req.chakra) do
          throwError (InvalidRequest "Invalid job type")
        QDBJ.markAsComplete oldJobId
  let kaalChakraHandle = Chakras.mkKaalChakraHandle mbMerchantId mbMerchantOpCityId
  case req.action of
    LYT.RUN -> YudhishthiraFlow.postRunKaalChakraJob kaalChakraHandle req
    LYT.SCHEDULE scheduledTime -> do
      now <- getCurrentTime
      when (scheduledTime <= now) $
        throwError (InvalidRequest "Schedule job available only for future")
      case req.usersSet of
        LYT.ALL_USERS -> pure ()
        _ -> throwError (InvalidRequest "Schedule job available only for all users")
      let jobData = LYT.mkKaalChakraJobData req (Just scheduledTime)
      kaalChakraHandle.createFetchUserDataJob req.chakra jobData scheduledTime

      logInfo $ "Scheduled new " <> show req.chakra <> " job"
      pure $ LYT.RunKaalChakraJobRes {eventId = Nothing, tags = Nothing, users = Nothing, chakraBatchState = LYT.Completed}
  where
    castChakra :: AllocatorJobType -> Maybe LYT.Chakra
    castChakra Daily = Just LYT.Daily
    castChakra Weekly = Just LYT.Weekly
    castChakra Monthly = Just LYT.Monthly
    castChakra Quarterly = Just LYT.Quarterly
    castChakra _ = Nothing

getNammaTagTimeBounds :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Environment.Flow LYT.TimeBoundResp
getNammaTagTimeBounds merchantShortId opCity domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getTimeBounds (cast merchantOpCityId) domain

postNammaTagTimeBoundsCreate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateTimeBoundRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagTimeBoundsCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.createTimeBounds (cast merchantOpCityId) req

deleteNammaTagTimeBoundsDelete :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteNammaTagTimeBoundsDelete merchantShortId opCity domain name = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.deleteTimeBounds (cast merchantOpCityId) domain name

getNammaTagAppDynamicLogicGetLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Maybe Bool -> Maybe Text -> LYT.LogicDomain -> Environment.Flow [LYT.LogicRolloutObject]
getNammaTagAppDynamicLogicGetLogicRollout merchantShortId opCity activeOnly timeBound domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getLogicRollout (cast merchantOpCityId) timeBound activeOnly domain

postNammaTagAppDynamicLogicUpsertLogicRollout :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> [LYT.LogicRolloutObject] -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagAppDynamicLogicUpsertLogicRollout merchantShortId opCity rolloutReq = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  result <- YudhishthiraFlow.upsertLogicRollout (Just $ cast merchant.id) (cast merchantOpCityId) rolloutReq TC.returnConfigs opCity
  forM_ rolloutReq $ \rolloutObj -> case rolloutObj.domain of
    LYT.DRIVER_CONFIG cfgType -> do
      TDL.deleteConfigHashKey (cast merchantOpCityId) rolloutObj.domain
      invalidateConfigInMem (TC.toCacheConfigType cfgType)
      logDebug $ "CP Log: Cleared Cache for " <> show cfgType
    _ -> pure ()
  pure result

getNammaTagAppDynamicLogicVersions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Int -> Prelude.Maybe Prelude.Int -> LYT.LogicDomain -> Environment.Flow LYT.AppDynamicLogicVersionResp
getNammaTagAppDynamicLogicVersions merchantShortId opCity mbLimit mbOffset domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getAppDynamicLogicVersions (cast merchantOpCityId) mbLimit mbOffset domain

getNammaTagAppDynamicLogicDomains :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow LYT.AppDynamicLogicDomainResp
getNammaTagAppDynamicLogicDomains _merchantShortId _opCity = return LYT.allValues

getNammaTagAppDynamicLogicDomainsAndEvents ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Bool ->
  Environment.Flow LYT.NammaTagEventsOrNammaTagNamesResp
getNammaTagAppDynamicLogicDomainsAndEvents merchantShortId opCity mbFetchNammaTagNames =
  case mbFetchNammaTagNames of
    Just True -> do
      merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
      let mocId = cast merchantOperatingCity.id
      tags <- QNammaTagV2.findAllByMerchantOperatingCityId mocId
      return $ LYT.NammaTagNames (map (.name) tags)
    _ -> return $ LYT.NammaTagEvents LYT.allValues

getNammaTagAppDynamicLogicGetDomainSchema :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Environment.Flow LYT.DomainSchemaResp
getNammaTagAppDynamicLogicGetDomainSchema _mrchntShortId _opCity domain = do
  case domain of
    LYT.POOLING ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: TaggedDriverPoolInput),
            LYT.schema = toInlinedSchemaValue (Proxy @TaggedDriverPoolInput)
          }
    LYT.CANCELLATION_COIN_POLICY ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: CancellationCoinData),
            LYT.schema = toInlinedSchemaValue (Proxy @CancellationCoinData)
          }
    LYT.DYNAMIC_PRICING_UNIFIED ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: DynamicPricingData),
            LYT.schema = toInlinedSchemaValue (Proxy @DynamicPricingData)
          }
    LYT.USER_CANCELLATION_DUES ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: UserCancellationDuesData),
            LYT.schema = toInlinedSchemaValue (Proxy @UserCancellationDuesData)
          }
    LYT.GPS_TOLL_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.CANCELLATION_RATE_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.ISSUE_BREACH_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.DRUNK_DRIVE_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.TOLL_ISSUE_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.AC_RESTRICTION_BEHAVIOR ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: BTT.BehaviorSnapshot),
            LYT.schema = toInlinedSchemaValue (Proxy @BTT.BehaviorSnapshot)
          }
    LYT.USER_CANCELLATION_DUES_WAIVE_OFF ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: UserCancellationDuesWaiveOffData),
            LYT.schema = toInlinedSchemaValue (Proxy @UserCancellationDuesWaiveOffData)
          }
    LYT.DRIVER_CONFIG LYT.DriverPoolConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "DriverPoolConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTD.DriverPoolConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTD.DriverPoolConfig))
          }
    LYT.DRIVER_CONFIG LYT.PayoutConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "PayoutConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTP.PayoutConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTP.PayoutConfig))
          }
    LYT.DRIVER_CONFIG LYT.RideRelatedNotificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "RideRelatedNotificationConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTRN.RideRelatedNotificationConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTRN.RideRelatedNotificationConfig))
          }
    LYT.DRIVER_CONFIG LYT.MerchantPushNotification -> do
      defaultConfig <- fromMaybeM (InvalidRequest "MerchantPushNotification default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTPN.MerchantPushNotification))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTPN.MerchantPushNotification))
          }
    LYT.INVOICE_TEMPLATE _scope ->
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (def :: FRT.InvoiceContext),
            LYT.schema = toInlinedSchemaValue (Proxy @FRT.InvoiceContext)
          }
    LYT.DRIVER_CONFIG LYT.MerchantServiceUsageConfigDriver -> do
      defaultConfig <- fromMaybeM (InvalidRequest "MerchantServiceUsageConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DMSUC.MerchantServiceUsageConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DMSUC.MerchantServiceUsageConfig))
          }
    LYT.DRIVER_CONFIG LYT.DocumentVerificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "DocumentVerificationConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DDVC.DocumentVerificationConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DDVC.DocumentVerificationConfig))
          }
    LYT.DRIVER_CONFIG LYT.GoHomeConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "GoHomeConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DGHC.GoHomeConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DGHC.GoHomeConfig))
          }
    LYT.DRIVER_CONFIG LYT.LeaderBoardConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "LeaderBoardConfigs default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DLBC.LeaderBoardConfigs))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DLBC.LeaderBoardConfigs))
          }
    LYT.DRIVER_CONFIG LYT.ReminderConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "ReminderConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DRMC.ReminderConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DRMC.ReminderConfig))
          }
    LYT.DRIVER_CONFIG LYT.ScheduledPayoutConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "ScheduledPayoutConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DSPC.ScheduledPayoutConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DSPC.ScheduledPayoutConfig))
          }
    LYT.DRIVER_CONFIG LYT.TagActionNotificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "TagActionNotificationConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTANC.TagActionNotificationConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTANC.TagActionNotificationConfig))
          }
    LYT.DRIVER_CONFIG LYT.FleetOwnerDocumentVerificationConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "FleetOwnerDocumentVerificationConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DFODVC.FleetOwnerDocumentVerificationConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DFODVC.FleetOwnerDocumentVerificationConfig))
          }
    LYT.DRIVER_CONFIG LYT.CoinsConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "CoinsConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DCC.CoinsConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DCC.CoinsConfig))
          }
    LYT.DRIVER_CONFIG LYT.TransporterConfig -> do
      defaultConfig <- fromMaybeM (InvalidRequest "TransporterConfig default config not found") (Prelude.listToMaybe $ YTH.genDef (Proxy @DTT.TransporterConfig))
      return $
        LYT.DomainSchemaResp
          { LYT.defaultValue = A.toJSON (LYT.Config defaultConfig Nothing 1),
            LYT.schema = toInlinedSchemaValue (Proxy @(LYT.Config DTT.TransporterConfig))
          }
    _ -> throwError $ InvalidRequest "Domain schema not available"

getNammaTagQueryAll :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.Chakra -> Environment.Flow LYT.ChakraQueryResp
getNammaTagQueryAll _merchantShortId _opCity = YudhishthiraFlow.getNammaTagQueryAll

postNammaTagConfigPilotGetVersion :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow LYT.UiConfigGetVersionResponse
postNammaTagConfigPilotGetVersion _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  config <- QUiConfig.findUIConfig uicr merchantOpCityId False
  baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
  let baseVersion = baseRollout.version
  case config of
    Just (_, version) -> pure $ LYT.UiConfigGetVersionResponse {version = Text.pack $ show version, baseVersion = Text.pack $ show baseVersion}
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotGetConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiConfigRequest -> Environment.Flow LYT.UiConfigResponse
postNammaTagConfigPilotGetConfig _ _ uicr = do
  merchant <- findById (Id uicr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just uicr.city)
  configInfo <- QUiConfig.findUIConfig uicr merchantOpCityId False
  isExp <- TDL.isExperimentRunning (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform)
  baseRollout <- CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) (LYT.UI_DRIVER uicr.os uicr.platform) >>= fromMaybeM (InvalidRequest "Base Rollout not found")
  let baseVersion = baseRollout.version
  case configInfo of
    Just (cfg, version) -> do
      pure (LYT.UiConfigResponse cfg.config (Text.pack $ show version) (Text.pack $ show baseVersion) isExp)
    Nothing -> throwError $ InternalError $ "No config found for merchant:" <> show uicr.merchantId <> " and city:" <> show uicr.city <> " and request:" <> show uicr

postNammaTagConfigPilotCreateUiConfig :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.CreateConfigRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotCreateUiConfig _ _ ccr = do
  url <- TC.getTSServiceUrl
  when (ccr.platform == LYT.TypeScript) $ do
    configValidateResp <- CPF.configValidate url (ccr.config)
    case configValidateResp.status of
      CPT.VALID_CONFIG -> pure ()
      CPT.INVALID_CONFIG -> throwError $ InvalidRequest "Invalid config"
      CPT.INVALID_REQUEST -> throwError $ InvalidRequest "Invalid request"
  merchant <- findById (Id ccr.merchantId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just ccr.city)
  now <- getCurrentTime
  id' <- generateGUID
  let uicr = LYT.UiConfigRequest {merchantId = ccr.merchantId, city = ccr.city, os = ccr.os, platform = ccr.platform, bundle = ccr.bundle, language = Nothing, toss = Nothing}
  configInfo <- QUiConfig.findUIConfig uicr merchantOpCityId False
  when (isJust configInfo) $ do
    throwError $ InvalidRequest "Config already exists"
  QUiConfig.create $ cfg merchantOpCityId now id'
  return Kernel.Types.APISuccess.Success
  where
    cfg merchantOpCityId now id' =
      DTDC.UiDriverConfig
        { platform = ccr.platform,
          config = ccr.config,
          createdAt = now,
          id = id',
          os = ccr.os,
          updatedAt = now,
          merchantOperatingCityId = merchantOpCityId
        }

getNammaTagConfigPilotAllConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [LYT.ConfigType]
getNammaTagConfigPilotAllConfigs _merchantShortId _opCity mbUnderExp = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAllConfigs (cast merchantOpCityId) mbUnderExp LYT.DriverCfg

getNammaTagConfigPilotConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow [LYT.ConfigDetailsResp]
getNammaTagConfigPilotConfigDetails _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotConfigDetails (cast merchantOpCityId) (LYT.DRIVER_CONFIG configType)

getNammaTagConfigPilotAlwaysOnList :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.LogicDomain -> Environment.Flow LYT.AlwaysOnListResp
getNammaTagConfigPilotAlwaysOnList merchantShortId opCity domain = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAlwaysOnList (cast merchantOpCityId) domain

getNammaTagConfigPilotGetTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow LYT.TableDataResp
getNammaTagConfigPilotGetTableData _merchantShortId _opCity configType = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.DRIVER_CONFIG configType
  TC.returnConfigs domain (cast merchantOpCityId) (cast merchant.id) _opCity

postNammaTagConfigPilotActionChange :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ActionChangeRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotActionChange _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  result <- YudhishthiraFlow.postNammaTagConfigPilotActionChange (Just $ cast merchant.id) (cast merchantOpCityId) req TC.handleConfigDBUpdate TC.returnConfigs _opCity
  let domain = case req of
        LYT.Conclude c -> c.domain
        LYT.Abort a -> a.domain
        LYT.Revert r -> r.domain
  case domain of
    LYT.DRIVER_CONFIG cfgType -> do
      TDL.deleteConfigHashKey (cast merchantOpCityId) domain
      invalidateConfigInMem (TC.toCacheConfigType cfgType)
      logDebug $ "CP Log: Cleared Cache for " <> show cfgType
    _ -> pure ()
  pure result

getNammaTagConfigPilotAllUiConfigs :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Prelude.Maybe Prelude.Bool -> Environment.Flow [LYT.LogicDomain]
getNammaTagConfigPilotAllUiConfigs _merchantShortId _opCity mbUnderExp = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.getNammaTagConfigPilotAllUiConfigs (cast merchantOpCityId) mbUnderExp LYT.DriverCfg

getNammaTagConfigPilotUiConfigDetails :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiDevicePlatformReq -> Environment.Flow [LYT.ConfigDetailsResp]
getNammaTagConfigPilotUiConfigDetails _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.UI_DRIVER req.deviceType req.platformType
  YudhishthiraFlow.getNammaTagConfigPilotUiConfigDetails (cast merchantOpCityId) domain

getNammaTagConfigPilotGetUiTableData :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.UiDevicePlatformReq -> Environment.Flow LYT.TableDataResp
getNammaTagConfigPilotGetUiTableData _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let domain = LYT.UI_DRIVER req.deviceType req.platformType
  TC.returnConfigs domain (cast merchantOpCityId) (cast merchant.id) _opCity

postNammaTagConfigPilotGetPatchedElement :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.GetPatchedElementReq -> Environment.Flow LYT.GetPatchedElementResp
postNammaTagConfigPilotGetPatchedElement _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  YudhishthiraFlow.postNammaTagConfigPilotGetPatchedElement (cast merchantOpCityId) req

postNammaTagConfigPilotGetConfigWithDimensions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigPilotGetConfigRequest -> Environment.Flow LYT.TableDataResp
postNammaTagConfigPilotGetConfigWithDimensions _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  let mocId = merchantOpCityId.getId
      dims = parseDims req.dimensions
  case req.configType of
    LYT.DriverPoolConfig -> do
      cfgs <- getConfig (DriverPoolConfigDimensions {merchantOperatingCityId = mocId, tripDistance = dimLookup "tripDistance" dims, area = dimLookup "area" dims, vehicleVariant = dimLookup "vehicleVariant" dims, tripCategory = dimLookup "tripCategory" dims}) (Just (SQDPC.findAllByMerchantOpCityId Nothing Nothing merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.TransporterConfig -> do
      cfg <- getConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId}) (Just (SQTC.findByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON (maybeToList cfg)}
    LYT.PayoutConfig -> do
      cfgs <- getConfig (PayoutConfigDimensions {merchantOperatingCityId = mocId, vehicleCategory = dimLookup "vehicleCategory" dims, isPayoutEnabled = dimLookup "isPayoutEnabled" dims}) (Just (SQPC.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.RideRelatedNotificationConfig -> do
      cfgs <- getConfig (RideRelatedNotificationConfigDimensions {merchantOperatingCityId = mocId, timeDiffEvent = dimLookup "timeDiffEvent" dims}) (Just (SQRRNC.findAllByMerchantOperatingCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.MerchantMessage -> do
      cfgs <- getConfig (MerchantMessageDimensions {merchantOperatingCityId = mocId, messageKey = dimLookup "messageKey" dims, vehicleCategory = dimLookup "vehicleCategory" dims}) (Just (SQMM.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.MerchantPushNotification -> do
      cfgs <- getConfig (MerchantPushNotificationDimensions {merchantOperatingCityId = mocId, key = dimLookup "key" dims, tripCategory = dimLookup "tripCategory" dims}) (Just (SQMPN.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.MerchantServiceUsageConfigDriver -> do
      cfg <- getConfig (MerchantServiceUsageConfigDimensions {merchantOperatingCityId = mocId}) (Just (SQMSUC.findByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON (maybeToList cfg)}
    LYT.DocumentVerificationConfig -> do
      cfgs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = mocId, documentType = dimLookup "documentType" dims, vehicleCategory = dimLookup "vehicleCategory" dims}) (Just (SQDVC.findAllByMerchantOpCityId Nothing Nothing merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.GoHomeConfig -> do
      cfg <- getConfig (GoHomeConfigDimensions {merchantOperatingCityId = mocId}) (Just (SQGHC.findByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON (maybeToList cfg)}
    LYT.LeaderBoardConfig -> do
      cfgs <- getConfig (LeaderBoardConfigsDimensions {merchantOperatingCityId = mocId, leaderBoardType = dimLookup "leaderBoardType" dims}) (Just (SQLBC.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.ReminderConfig -> do
      cfgs <- getConfig (ReminderConfigDimensions {merchantOperatingCityId = mocId, documentType = dimLookup "documentType" dims}) (Just (SQRMC.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.ScheduledPayoutConfig -> do
      cfgs <- getConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = mocId, isEnabled = dimLookup "isEnabled" dims, payoutCategory = dimLookup "payoutCategory" dims}) (Just (SQSPC.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.TagActionNotificationConfig -> do
      cfgs <- getConfig (TagActionNotificationConfigDimensions {merchantOperatingCityId = mocId, notificationKey = dimLookup "notificationKey" dims}) (Just (SQTANC.findAllByMerchantOperatingCityId (cast merchantOpCityId)))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.FleetOwnerDocumentVerificationConfig -> do
      cfgs <- getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = mocId, documentType = dimLookup "documentType" dims, role = dimLookup "role" dims}) (Just (SQFODVC.findAllByMerchantOpCityId Nothing Nothing merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.CoinsConfig -> do
      cfgs <- getConfig (CoinsConfigDimensions {merchantOptCityId = mocId, eventFunction = dimLookup "eventFunction" dims, merchantId = dimLookup "merchantId" dims, active = dimLookup "active" dims, vehicleCategory = dimLookup "vehicleCategory" dims, serviceTierType = dimLookup "serviceTierType" dims, eventName = dimLookup "eventName" dims, tripCategoryType = dimLookup "tripCategoryType" dims, configId = dimLookup "configId" dims}) (Just (SQCCfg.findAllByMerchantOptCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.MerchantServiceConfigDriver -> do
      cfgs <- getConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId, merchantId = dimLookup "merchantId" dims, serviceName = dimLookup "serviceName" dims}) (Just (SQMSCE.findAllMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.Exophone -> do
      cfgs <- getConfig (ExophoneDimensions {merchantOperatingCityId = mocId, phoneNumber = dimLookup "phoneNumber" dims, callService = dimLookup "callService" dims, exophoneType = dimLookup "exophoneType" dims}) (Just (SQEXO.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.Overlay -> do
      cfgs <- getConfig (OverlayDimensions {merchantOperatingCityId = mocId, overlayKey = dimLookup "overlayKey" dims, language = dimLookup "language" dims, udf1 = dimLookup "udf1" dims, vehicleCategory = dimLookup "vehicleCategory" dims}) (Just (SQOVL.findAllByMerchantOpCityId merchantOpCityId))
      pure LYT.TableDataResp {configs = map A.toJSON cfgs}
    LYT.Translation -> do
      cfg <- getConfig (TranslationDimensions {merchantOperatingCityId = Just mocId, messageKey = fromMaybe "" (dimLookup "messageKey" dims), language = dimLookup "language" dims}) (Just (Prelude.listToMaybe <$> SQTRE.findAllByMessageKey (fromMaybe "" (dimLookup "messageKey" dims))))
      pure LYT.TableDataResp {configs = map A.toJSON (maybeToList cfg)}
    LYT.IssueConfig -> do
      cfg <- getConfig (IssueConfigDimensions {merchantOperatingCityId = mocId, identifier = fromMaybe "" (dimLookup "identifier" dims)}) (Just (SQICfg.findByMerchantOpCityId (cast merchantOpCityId)))
      pure LYT.TableDataResp {configs = map A.toJSON (maybeToList cfg)}
    _ -> throwError $ InvalidRequest $ "Config type " <> show req.configType <> " is not supported for getConfigWithDimensions"
  where
    parseDims :: A.Value -> Maybe A.Object
    parseDims (A.Object o) = Just o
    parseDims _ = Nothing

    dimLookup :: A.FromJSON a => A.Key -> Maybe A.Object -> Maybe a
    dimLookup key obj = obj >>= AT.parseMaybe (A..: key)

getNammaTagConfigPilotGetDimensionSchema :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigType -> Environment.Flow LYT.DomainSchemaResp
getNammaTagConfigPilotGetDimensionSchema _merchantShortId _opCity configType =
  case configType of
    LYT.DriverPoolConfig -> pure $ mkDimSchema (Proxy @DriverPoolConfigDimensions)
    LYT.TransporterConfig -> pure $ mkDimSchema (Proxy @TransporterConfigDimensions)
    LYT.PayoutConfig -> pure $ mkDimSchema (Proxy @PayoutConfigDimensions)
    LYT.RideRelatedNotificationConfig -> pure $ mkDimSchema (Proxy @RideRelatedNotificationConfigDimensions)
    LYT.MerchantMessage -> pure $ mkDimSchema (Proxy @MerchantMessageDimensions)
    LYT.MerchantPushNotification -> pure $ mkDimSchema (Proxy @MerchantPushNotificationDimensions)
    LYT.MerchantServiceUsageConfigDriver -> pure $ mkDimSchema (Proxy @MerchantServiceUsageConfigDimensions)
    LYT.DocumentVerificationConfig -> pure $ mkDimSchema (Proxy @DocumentVerificationConfigDimensions)
    LYT.GoHomeConfig -> pure $ mkDimSchema (Proxy @GoHomeConfigDimensions)
    LYT.LeaderBoardConfig -> pure $ mkDimSchema (Proxy @LeaderBoardConfigsDimensions)
    LYT.ReminderConfig -> pure $ mkDimSchema (Proxy @ReminderConfigDimensions)
    LYT.ScheduledPayoutConfig -> pure $ mkDimSchema (Proxy @ScheduledPayoutConfigDimensions)
    LYT.TagActionNotificationConfig -> pure $ mkDimSchema (Proxy @TagActionNotificationConfigDimensions)
    LYT.FleetOwnerDocumentVerificationConfig -> pure $ mkDimSchema (Proxy @FleetOwnerDocumentVerificationConfigDimensions)
    LYT.CoinsConfig -> pure $ mkDimSchema (Proxy @CoinsConfigDimensions)
    LYT.MerchantServiceConfigDriver -> pure $ mkDimSchema (Proxy @MerchantServiceConfigDimensions)
    LYT.Exophone -> pure $ mkDimSchema (Proxy @ExophoneDimensions)
    LYT.Overlay -> pure $ mkDimSchema (Proxy @OverlayDimensions)
    LYT.Translation -> pure $ mkDimSchema (Proxy @TranslationDimensions)
    LYT.IssueConfig -> pure $ mkDimSchema (Proxy @IssueConfigDimensions)
    LYT.UiDriverConfig -> pure $ mkDimSchema (Proxy @UiDriverConfigDimensions)
    _ -> throwError $ InvalidRequest $ "Dimension schema not available for " <> show configType
  where
    mkDimSchema :: forall a. (A.ToJSON a, Prelude.ToSchema a) => Proxy a -> LYT.DomainSchemaResp
    mkDimSchema p =
      LYT.DomainSchemaResp
        { LYT.defaultValue = A.Null,
          LYT.schema = toInlinedSchemaValue p
        }

postNammaTagConfigPilotCreateRow :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> LYT.ConfigPilotCreateRowRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postNammaTagConfigPilotCreateRow _merchantShortId _opCity req = do
  merchant <- findMerchantByShortId _merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just _opCity)
  case req.configType of
    LYT.DriverPoolConfig -> do
      cfg :: DTD.DriverPoolConfig <- parseConfigData req.configData
      SQDPC.create cfg
      invalidateConfigInMem LYT.DriverPoolConfig
    LYT.TransporterConfig -> do
      cfg :: DTT.TransporterConfig <- parseConfigData req.configData
      existing <- SQTC.findByMerchantOpCityId merchantOpCityId
      when (isJust existing) $ throwError $ InvalidRequest "TransporterConfig already exists for this merchantOperatingCityId"
      SQTC.create cfg
      invalidateConfigInMem LYT.TransporterConfig
    LYT.PayoutConfig -> do
      cfg :: DTP.PayoutConfig <- parseConfigData req.configData
      SQPC.create cfg
      invalidateConfigInMem LYT.PayoutConfig
    LYT.RideRelatedNotificationConfig -> do
      cfg :: DTRN.RideRelatedNotificationConfig <- parseConfigData req.configData
      SQRRNC.create cfg
      invalidateConfigInMem LYT.RideRelatedNotificationConfig
    LYT.MerchantMessage -> do
      cfg :: DTM.MerchantMessage <- parseConfigData req.configData
      SQMM.create cfg
      invalidateConfigInMem LYT.MerchantMessage
    LYT.MerchantPushNotification -> do
      cfg :: DTPN.MerchantPushNotification <- parseConfigData req.configData
      SQMPN.create cfg
      invalidateConfigInMem LYT.MerchantPushNotification
    LYT.MerchantServiceUsageConfigDriver -> do
      cfg :: DMSUC.MerchantServiceUsageConfig <- parseConfigData req.configData
      existing <- SQMSUC.findByMerchantOpCityId merchantOpCityId
      when (isJust existing) $ throwError $ InvalidRequest "MerchantServiceUsageConfig already exists for this merchantOperatingCityId"
      SQMSUC.create cfg
      invalidateConfigInMem LYT.MerchantServiceUsageConfigDriver
    LYT.DocumentVerificationConfig -> do
      cfg :: DDVC.DocumentVerificationConfig <- parseConfigData req.configData
      SQDVC.create cfg
      invalidateConfigInMem LYT.DocumentVerificationConfig
    LYT.GoHomeConfig -> do
      cfg :: DGHC.GoHomeConfig <- parseConfigData req.configData
      existing <- SQGHC.findByMerchantOpCityId merchantOpCityId
      when (isJust existing) $ throwError $ InvalidRequest "GoHomeConfig already exists for this merchantOperatingCityId"
      SQGHC.create cfg
      invalidateConfigInMem LYT.GoHomeConfig
    LYT.LeaderBoardConfig -> do
      cfg :: DLBC.LeaderBoardConfigs <- parseConfigData req.configData
      SQLBC.create cfg
      invalidateConfigInMem LYT.LeaderBoardConfig
    LYT.ReminderConfig -> do
      cfg :: DRMC.ReminderConfig <- parseConfigData req.configData
      SQRMC.create cfg
      invalidateConfigInMem LYT.ReminderConfig
    LYT.ScheduledPayoutConfig -> do
      cfg :: DSPC.ScheduledPayoutConfig <- parseConfigData req.configData
      SQSPC.create cfg
      invalidateConfigInMem LYT.ScheduledPayoutConfig
    LYT.FleetOwnerDocumentVerificationConfig -> do
      cfg :: DFODVC.FleetOwnerDocumentVerificationConfig <- parseConfigData req.configData
      SQFODVC.create cfg
      invalidateConfigInMem LYT.FleetOwnerDocumentVerificationConfig
    LYT.TagActionNotificationConfig -> do
      cfg :: DTANC.TagActionNotificationConfig <- parseConfigData req.configData
      SQTANC.create cfg
      invalidateConfigInMem LYT.TagActionNotificationConfig
    LYT.Exophone -> do
      cfg :: DTEXO.Exophone <- parseConfigData req.configData
      SQEXO.create cfg
      invalidateConfigInMem LYT.Exophone
    LYT.Overlay -> do
      cfg :: DTOVL.Overlay <- parseConfigData req.configData
      SQOVL.create cfg
      invalidateConfigInMem LYT.Overlay
    LYT.Translation -> do
      cfg :: DTTR.Translations <- parseConfigData req.configData
      SQTR.create cfg
      invalidateConfigInMem LYT.TranslationDriver
    LYT.IssueConfig -> do
      cfg :: DIC.IssueConfig <- parseConfigData req.configData
      SQICfg.create cfg
      invalidateConfigInMem LYT.IssueConfigDriver
    LYT.UiDriverConfig -> do
      cfg :: DTDC.UiDriverConfig <- parseConfigData req.configData
      SQU.create cfg
      invalidateConfigInMem LYT.UiDriverConfig
    _ -> throwError $ InvalidRequest $ "Config type " <> show req.configType <> " is not supported for createRow"
  pure Kernel.Types.APISuccess.Success
  where
    parseConfigData :: forall a m. (A.FromJSON a, MonadFlow m) => A.Value -> m a
    parseConfigData val = case A.fromJSON val of
      A.Success cfg -> pure cfg
      A.Error err -> throwError $ InvalidRequest $ "Invalid config data: " <> show err

-- { os :: DeviceType,
--     language :: Language,
--     bundle :: Maybe Text,
--     platform :: PlatformType,
--     merchantId :: Text,
--     city :: Kernel.Types.Beckn.Context.City,
--     toss :: Maybe Int

-- LYT.UI_DRIVER dt pt -> do
--   uiDriverCfg <- QUiC.findUIConfig (LYT.UiConfigRequest dt Nothing Nothing pt (cast merchant.id.getId) (cast merchantOpCityId)) merchantOpCityId
--   let configWrapper :: [LYT.Config DTT.TransporterConfig] =
--         zipWith
--           (\id cfg -> cfg {LYT.identifier = id})
--           [0 ..]
--           [(\cfg -> LYT.Config {config = cfg, extraDimensions = Nothing, identifier = 0}) transporterCfg]
--   patchedConfigs <- mapM (LYTU.runLogics baseLogics) configWrapper
--   mbCfgs :: [Maybe (LYT.Config DTT.TransporterConfig)] <-
--     mapM
--       ( \resp ->
--           case (A.fromJSON resp.result :: A.Result (LYT.Config DTT.TransporterConfig)) of
--             A.Success tc -> pure $ Just tc
--             A.Error e -> throwError $ InvalidRequest $ "Error occurred while applying JSON patch to transporter config. " <> show e
--       )
--       patchedConfigs
--   let configsToUpdate :: [DTT.TransporterConfig] =
--         catMaybes $
--           zipWith
--             ( \cfg mbCfg ->
--                 case mbCfg of
--                   Just cfg' | cfg /= cfg' -> Just cfg'.config
--                   _ -> Nothing
--             )
--             configWrapper
--             mbCfgs
--   mapM_ SCMT.update configsToUpdate

-- | Get behavior visibility for any entity (driver/rider).
-- Returns all tracked counters, active blocks (with TTL), and active cooldowns.
-- Config-driven: no Redis SCAN, queries known action types and reason tags.
getNammaTagBehaviorVisibility ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text -> -- entityType: "DRIVER" or "RIDER"
  Text -> -- entityId
  Environment.Flow BTT.EntityBehaviorVisibility
getNammaTagBehaviorVisibility merchantShortId opCity entityTypeText entityId = do
  entityType <- case entityTypeText of
    "DRIVER" -> pure BTT.DRIVER
    "RIDER" -> pure BTT.RIDER
    other -> throwError $ InvalidRequest $ "Invalid entityType: " <> other <> ". Expected DRIVER or RIDER."
  merchant <- findMerchantByShortId merchantShortId
  _merchantOpCityId <- CQMOC.getMerchantOpCityId Prelude.Nothing merchant (Prelude.Just opCity)
  BehaviorVisibility.getEntityBehaviorVisibility entityType entityId Prelude.Nothing

mkNammaTagDetailsResp :: Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2 -> [LYT.ApplicationEvent] -> Map.Map Text (Maybe A.Value) -> LYT.NammaTagDetailsResp
mkNammaTagDetailsResp tag events inputDataMap =
  LYT.NammaTagDetailsResp
    { actionEngine = tag.actionEngine,
      category = tag.category,
      description = tag.description,
      tagInfo = TE.decodeUtf8 $ BSL.toStrict $ A.encode tag.info,
      name = tag.name,
      possibleValues = tag.possibleValues,
      rule = tag.rule,
      validity = tag.validity,
      tagStages = events,
      defaultInputDataPerEvent = inputDataMap,
      createdAt = tag.createdAt,
      updatedAt = tag.updatedAt
    }
