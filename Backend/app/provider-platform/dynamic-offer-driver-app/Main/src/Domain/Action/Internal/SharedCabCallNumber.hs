-- | rider-app asks for a shared-cab driver's phone when a rider dials the ExoPhone. rider-app decides who may call whom; this only
-- authenticates the caller with the driver's merchant key and hands the number over for that one call.
module Domain.Action.Internal.SharedCabCallNumber where

import Data.OpenApi (ToSchema)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Person as QPerson

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
  decrypted <- mapM decrypt person.mobileNumber
  number <- fromMaybeM (InternalError "Driver has no phone number.") $ (<>) <$> person.mobileCountryCode <*> decrypted
  pure $ SharedCabDriverCallNumberResp number
