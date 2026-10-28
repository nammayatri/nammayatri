-- | rider-app asks for a shared-cab driver's phone when a rider dials the ExoPhone. rider-app decides who may call whom; this only
-- authenticates the caller with the driver's merchant key and hands the number over for that one call, and only for a shared-cab driver.
module Domain.Action.Internal.SharedCabCallNumber where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as SP
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle

newtype SharedCabDriverCallNumberResp = SharedCabDriverCallNumberResp {driverNumber :: Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

sharedCabDriverCallNumber :: Text -> Maybe Text -> Flow SharedCabDriverCallNumberResp
sharedCabDriverCallNumber driverId apiKey = do
  -- same order as SharedCabAllocation: an unknown driver, an unknown merchant and a wrong key all answer AuthBlocked
  mbPerson <- QPerson.findById (Id driverId)
  mbMerchant <- maybe (pure Nothing) (QM.findById . (.merchantId)) mbPerson
  person <- case (mbPerson, mbMerchant) of
    (Just p, Just m) | Just m.internalApiKey == apiKey -> pure p
    _ -> throwError $ AuthBlocked "Invalid BPP internal api key"
  mbVehicle <- QVehicle.findById person.id
  unless (callNumberAllowed person.role ((.variant) <$> mbVehicle)) $ throwError $ AuthBlocked "Invalid BPP internal api key"
  decrypted <- mapM decrypt person.mobileNumber
  number <- fromMaybeM (InternalError "Driver has no phone number.") $ (<>) <$> person.mobileCountryCode <*> decrypted
  pure $ SharedCabDriverCallNumberResp number

-- | Only a driver whose linked vehicle is a shared cab has a number to hand out; anyone else answers as an unknown person.
callNumberAllowed :: SP.Role -> Maybe DV.VehicleVariant -> Bool
callNumberAllowed role mbVariant = role == SP.DRIVER && mbVariant == Just DV.SHARED_CAB
