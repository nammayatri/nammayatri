module Domain.Action.UI.DriverOnboarding.CourtRecordCheck
  ( runCourtRecordCheck,
    onVerifyCRC,
  )
where

import Data.Time (utctDay)
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
    if isJust (mbIdentityInfo >>= (.courtRecord))
      then logInfo $ "CourtRecordCheck: court record already present for driver " <> person.id.getId <> ", skipping submit"
      else do
        mbPanNumber <- mapM decrypt driverInfo.panNumber
        let driverName = person.firstName <> maybe "" (" " <>) person.lastName
            mbDob = utctDay <$> driverInfo.driverDob
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
        resp <- Verification.verifyCRCAsync person.merchantId merchantOpCity.id req
        logInfo $ "CourtRecordCheck: submitted CRC for driver " <> person.id.getId <> " requestId=" <> resp.requestId

-- | Persist the CRC result JSON on driver_identity_info.court_record. Invoked
-- from the Idfy webhook when the court record check result arrives.
onVerifyCRC :: Id Person.Person -> VT.CRCVerificationResponse -> Flow ()
onVerifyCRC driverId crcOutput = do
  QDII.updateCourtRecord (Just crcOutput) driverId
  logInfo $ "CourtRecordCheck: stored court record for driver " <> driverId.getId
