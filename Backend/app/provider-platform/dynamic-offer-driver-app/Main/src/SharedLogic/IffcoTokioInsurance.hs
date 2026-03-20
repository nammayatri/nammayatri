module SharedLogic.IffcoTokioInsurance
  ( triggerIffcoTokioInsurance,
  )
where

import qualified Data.Text as Text
import qualified Data.Time.Calendar as Cal
import qualified Domain.Types.IffcoTokioInsurance as DITI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment ()
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Insurance.Interface as Insurance
import qualified Kernel.External.Insurance.Interface.Types as InsuranceTypes
import qualified Kernel.External.Insurance.IffcoTokio.Types as IffcoTokio
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude ()
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant.Client.Core ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.IffcoTokioInsurance as QIffco
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import Data.Time (UTCTime (..), utctDay)
import Data.Time.Calendar (addDays)

-- | Trigger IffcoTokio insurance for a driver at ride start.
-- Skips if (1) config not set for the city, (2) driver already insured today (same local calendar day),
-- or (3) nomineeName / nomineeRelationship is missing in driver_information.
-- "Today" uses the city's timezone (transporterConfig.timeDiffFromUtc).
-- Creates a PENDING entry, then asynchronously updates it to INSURED or FAILED.
triggerIffcoTokioInsurance ::
  ( EncFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    MonadTime m,
    Log m,
    MonadReader r m
  ) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m ()
triggerIffcoTokioInsurance driverId merchantId merchantOpCityId = do
  msc <- CQMSC.findByServiceAndCity (ExtraMSC.InsuranceDeclarationService ExtraMSC.IffcoTokio) merchantOpCityId
  let mbIffcoExtCfg = case msc of
        Just x -> case x.serviceConfig of
          ExtraMSC.InsuranceDeclarationServiceConfig cfg -> Just cfg
          _ -> Nothing
        Nothing -> Nothing
  whenJust mbIffcoExtCfg $ \iffcoExtCfg -> do
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    now <- getCurrentTime
    let todayLocal = utctDay localTime
        timeOffset = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
        startOfTodayUTC = addUTCTime (-timeOffset) (UTCTime todayLocal 0)
        startOfTomorrowUTC = addUTCTime 86400 startOfTodayUTC
    mbExisting <- QIffco.findLatestByDriverId driverId
    let alreadyInsured = case mbExisting of
          Just entry ->
            (entry.insuranceStatus == DITI.PENDING || entry.insuranceStatus == DITI.INSURED)
              && entry.createdAt >= startOfTodayUTC
              && entry.createdAt < startOfTomorrowUTC
          Nothing -> False
    unless alreadyInsured $ do
      person <- QPerson.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
      driverInfo <- QDI.findByPrimaryKey (cast driverId) >>= fromMaybeM DriverInfoNotFound
      let nomineeName = driverInfo.nomineeName
          nomineeRelationship = driverInfo.nomineeRelationship
      if isNothing nomineeName || isNothing nomineeRelationship
        then logInfo $ "IffcoTokio: skipping insurance for driver=" <> driverId.getId <> " nomineeName or nomineeRelationship is missing"
        else do
          decPerson <- decrypt person
          mobileNumber <- decPerson.mobileNumber & fromMaybeM (InvalidRequest "Driver mobile number not found for IffcoTokio insurance")
          mbDriverLicense <- QDL.findByDriverId (cast driverId)
          dlNumber <- case mbDriverLicense of
            Just dl -> do
              decDl <- decrypt dl
              pure $ Just decDl.licenseNumber
            Nothing -> pure Nothing
          insId <- generateGUID
          invoiceReqNum <- generateAplhaNumbericCode 30
          let (yr, mon, day) = Cal.toGregorian todayLocal
              invoiceDateStr =
                Text.pack (show mon)
                  <> "/"
                  <> Text.pack (show day)
                  <> "/"
                  <> Text.pack (show yr)
              riskEndDateStr = invoiceDateStr <> " 11:59:59 PM"
          let insuranceEntry =
                DITI.IffcoTokioInsurance
                  { id = Id insId,
                    driverId = driverId,
                    merchantId = merchantId,
                    merchantOperatingCityId = merchantOpCityId,
                    invoiceRequestNumber = invoiceReqNum,
                    certificateNumber = Nothing,
                    declarationId = Nothing,
                    iffcoStatus = Nothing,
                    insuranceStatus = DITI.PENDING,
                    createdAt = now,
                    updatedAt = now
                  }
          QIffco.create insuranceEntry
          let iffcoCfg =
                IffcoTokio.IffcoTokioConfig
                  { url = iffcoExtCfg.url,
                    username = iffcoExtCfg.username,
                    password = iffcoExtCfg.password,
                    masterPolicyClient = iffcoExtCfg.masterPolicyClient,
                    insurancePlan = iffcoExtCfg.insurancePlan
                  }
              req =
                InsuranceTypes.HomeDeclarationReq
                  { insuredMobile = mobileNumber,
                    insuredName =
                      person.firstName
                        <> maybe "" (" " <>) person.lastName,
                    invoiceDate = invoiceDateStr,
                    invoiceRequestNumber = invoiceReqNum,
                    insuredAddress = dlNumber,
                    insuredEmail = Just iffcoExtCfg.insuredEmail,
                    ewCommencesOn = Nothing,
                    customerId = Nothing,
                    extraAttrib01 = nomineeName,
                    extraAttrib02 = nomineeRelationship,
                    insurancePlan = Just iffcoExtCfg.insurancePlan,
                    insuredProductCode = Nothing,
                    itemDetail = Nothing,
                    masterPolicyClient = Just iffcoExtCfg.masterPolicyClient,
                    riskEndDate = Just riskEndDateStr,
                    riskStartDate = Just invoiceDateStr,
                    sumInsured = Nothing
                  }
          logInfo $ "IffcoTokio: triggering insurance for driver=" <> driverId.getId <> " invoiceReqNum=" <> invoiceReqNum
          void $
            Insurance.registerHomeDeclaration iffcoCfg req $ \case
              Left err -> do
                logError $ "IffcoTokio async error for " <> invoiceReqNum <> ": " <> err
                QIffco.updateByInvoiceRequestNumber invoiceReqNum Nothing Nothing Nothing DITI.FAILED
              Right asyncResp -> do
                let insStatus =
                      if all
                        (not . Text.null)
                        [ asyncResp.certificateNumber,
                          asyncResp.declarationId,
                          asyncResp.status
                        ]
                        then DITI.INSURED
                        else DITI.FAILED
                logInfo $
                  "IffcoTokio async success for "
                    <> invoiceReqNum
                    <> ": certificate="
                    <> asyncResp.certificateNumber
                    <> " declarationId="
                    <> asyncResp.declarationId
                    <> " status="
                    <> asyncResp.status
                    <> " insuranceStatus="
                    <> show insStatus
                QIffco.updateByInvoiceRequestNumber
                  invoiceReqNum
                  (Just asyncResp.certificateNumber)
                  (Just asyncResp.declarationId)
                  (Just asyncResp.status)
                  insStatus
                when (insStatus == DITI.INSURED) $
                  Notify.notifyDriver
                    merchantOpCityId
                    FCM.NEW_MESSAGE
                    iffcoExtCfg.fcmNotificationTitle
                    iffcoExtCfg.fcmNotificationMessage
                    person
                    person.deviceToken
