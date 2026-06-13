module Domain.Action.UI.DriverOnboarding.CourtRecordCheck
  ( runCourtRecordCheck,
    onVerifyCRC,
  )
where

import Data.Time (utctDay)
import qualified Domain.Types.DriverIdentityInfo as DII
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Verification.Interface.Types as VI
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformationExtra as QDI
import qualified Tools.Verification as Verification

-- | Submit a Court Record Check (CRC) for a driver. Idfy returns a requestId
-- synchronously and pushes the result to the Idfy webhook when the check
-- completes (handled in IdfyWebhook.onVerify -> onVerifyCRC).
runCourtRecordCheck :: Person.Person -> DMOC.MerchantOperatingCity -> Flow ()
runCourtRecordCheck person merchantOpCity = do
  mbDriverInfo <- QDI.findById (cast person.id)
  mbIdentityInfo <- QDII.findByDriverId person.id
  -- The CRC result arrives asynchronously on the Idfy webhook (onVerifyCRC -> updateCourtRecord,
  -- an update-only KV op). Ensure a driver_identity_info row exists now so the result is not
  -- dropped for drivers who never set nominee/address info.
  when (isNothing mbIdentityInfo) $ do
    now <- getCurrentTime
    QDII.create
      DII.DriverIdentityInfo
        { driverId = person.id,
          nomineeName = Nothing,
          nomineeRelationship = Nothing,
          nomineeDob = Nothing,
          address = Nothing,
          addressDocumentType = Nothing,
          addressState = Nothing,
          courtRecord = Nothing,
          merchantId = person.merchantId,
          merchantOperatingCityId = merchantOpCity.id,
          createdAt = now,
          updatedAt = now
        }
  -- PAN is set on DriverInformation at PAN verification (PanVerification: updatePanNumber for DRIVER),
  -- so it's reliably present once docs are verified — reuse the row we already fetched (no extra query).
  mbPanNumber <- mapM decrypt (mbDriverInfo >>= (.panNumber))
  let driverName = person.firstName <> maybe "" (" " <>) person.lastName
      mbDob = utctDay <$> (mbDriverInfo >>= (.driverDob))
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
  QDII.updateCourtRecord (Just (toJSON crcOutput)) driverId
  logInfo $ "CourtRecordCheck: stored court record for driver " <> driverId.getId
