module Domain.Action.UI.Insurance (getInsurance, getDriverInsurance) where

import Data.Time (UTCTime (..), utctDay)
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAPInternal
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Storage.Queries.IffcoTokioInsurance as QIffco
import qualified Storage.Queries.Ride as QRide

-- | GET /ui/insurance/{referenceId}
-- Ride-level insurance: looks up the ride and calls BAP-internal for insurance info.
-- Backward compatible — unchanged from original flow.
getInsurance ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow SharedLogic.CallBAPInternal.InsuranceAPIEntity
  )
getInsurance (_mbPersonId, _merchantId, _merchantOpCityId) referenceId = do
  ride <- runInReplica $ QRide.findById (Kernel.Types.Id.Id referenceId) >>= fromMaybeM (RideDoesNotExist referenceId)
  unless (ride.isInsured) $ throwError $ InvalidRequest "This ride is not insured!"
  appBackendBapInternal <- asks (.appBackendBapInternal)
  SharedLogic.CallBAPInternal.getInsuranceInfo appBackendBapInternal.apiKey appBackendBapInternal.url referenceId

-- | GET /ui/driver/insurance
-- Driver-level insurance (IffcoTokio): returns today's insurance entry for the authenticated driver.
-- Throws if the city is not configured for IffcoTokio, or if no valid entry exists for today.
getDriverInsurance ::
  ( Kernel.Types.Id.Id Domain.Types.Person.Person,
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Environment.Flow SharedLogic.CallBAPInternal.InsuranceAPIEntity
getDriverInsurance (personId, _merchantId, merchantOpCityId) = do
  _ <-
    CQMSC.findByServiceAndCity (ExtraMSC.InsuranceDeclarationService ExtraMSC.IffcoTokio) merchantOpCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "InsuranceDeclaration_IffcoTokio" (show ExtraMSC.IffcoTokio))
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let todayLocal = utctDay localTime
      timeOffset = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      startOfToday = addUTCTime (-timeOffset) (UTCTime todayLocal 0)
      startOfTomorrow = addUTCTime 86400 startOfToday
  mbInsurance <- QIffco.findLatestByDriverId personId
  case mbInsurance of
    Just ins
      | ins.createdAt >= startOfToday && ins.createdAt < startOfTomorrow ->
        pure $
          SharedLogic.CallBAPInternal.InsuranceAPIEntity
            { certificateUrl = Nothing,
              message = fromMaybe "" ins.iffcoStatus,
              plan = Just "IffcoTokio",
              policyId = ins.declarationId,
              policyNumber = ins.certificateNumber
            }
    _ -> throwError $ InvalidRequest "No active insurance found for today"
