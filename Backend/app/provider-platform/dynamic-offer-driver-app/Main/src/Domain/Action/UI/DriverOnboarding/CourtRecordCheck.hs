module Domain.Action.UI.DriverOnboarding.CourtRecordCheck
  ( runCourtRecordCheck,
    onVerifyCRC,
    onVerifyCRCError,
  )
where

import Control.Applicative ((<|>))
import Data.Time (utctDay)
import qualified Domain.Types.DriverIdentityInfo as DDII
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Verification.Interface.Types as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverIdentityInfo as DIInfo
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformationExtra as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Tools.Verification as Verification

runCourtRecordCheck :: Person.Person -> DMOC.MerchantOperatingCity -> Flow ()
runCourtRecordCheck person merchantOpCity = do
  mbDriverInfo <- QDI.findById (cast person.id)
  whenJust mbDriverInfo $ \driverInfo -> do
    mbIdentityInfoPre <- QDII.findByDriverId person.id
    when (isNothing mbIdentityInfoPre) $
      Redis.withLockRedis (DIInfo.driverIdentityInfoLockKey person.id) 10 $ do
        mbExisting <- QDII.findByDriverId person.id
        when (isNothing mbExisting) $
          void $ DIInfo.upsertDriverIdentityInfo mbExisting person.id person.merchantId merchantOpCity.id driverInfo Nothing Nothing Nothing Nothing Nothing Nothing
    mbIdentityInfo <- QDII.findByDriverId person.id
    -- skip only when a successful result is already stored; a stored error should still retry
    if isJust (mbIdentityInfo >>= (.courtRecord) >>= (.result))
      then logInfo $ "CourtRecordCheck: court record already present for driver " <> person.id.getId <> ", skipping submit"
      else do
        mbPanCard <- QPanCard.findByDriverId person.id
        mbPanFromInfo <- mapM decrypt driverInfo.panNumber
        mbPanFromCard <- mapM (decrypt . (.panCardNumber)) mbPanCard
        let driverName = person.firstName <> maybe "" (" " <>) person.lastName
            mbDob = (utctDay <$> driverInfo.driverDob) <|> (utctDay <$> (mbPanCard >>= (.driverDob)))
            mbPanNumber = mbPanFromInfo <|> mbPanFromCard
            mbAddress = mbIdentityInfo >>= (.address)
            req =
              VI.VerifyCRCReq
                { name = driverName,
                  fatherName = Nothing,
                  dob = mbDob,
                  address = mbAddress,
                  panNumber = mbPanNumber,
                  entityType = VT.Individual,
                  driverId = person.id.getId
                }
        result <- withTryCatch "courtRecordCheck:submit" $ Verification.verifyCRCAsync person.merchantId merchantOpCity.id req
        case result of
          Right resp -> logInfo $ "CourtRecordCheck: submitted CRC for driver " <> person.id.getId <> " requestId=" <> resp.requestId
          Left err -> do
            logError $ "CourtRecordCheck: submit failed for driver " <> person.id.getId <> ": " <> show err
            onVerifyCRCError person.id (show err)

onVerifyCRC :: Id Person.Person -> VT.CRCVerificationResponse -> Flow ()
onVerifyCRC driverId crcOutput = do
  QDII.updateCourtRecord (Just DDII.CourtRecordResult {result = Just crcOutput, errorMessage = Nothing}) driverId
  logInfo $ "CourtRecordCheck: stored court record for driver " <> driverId.getId

onVerifyCRCError :: Id Person.Person -> Text -> Flow ()
onVerifyCRCError driverId errMsg = do
  QDII.updateCourtRecord (Just DDII.CourtRecordResult {result = Nothing, errorMessage = Just errMsg}) driverId
  logInfo $ "CourtRecordCheck: stored court record error for driver " <> driverId.getId
