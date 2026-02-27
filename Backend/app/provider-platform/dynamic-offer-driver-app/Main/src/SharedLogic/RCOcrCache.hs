{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Isolated Redis-cache helpers for RC OCR data extracted during validateDocumentImage.
-- Keeping this in a separate SharedLogic module avoids a cyclic import between
-- Domain.Action.UI.DriverOnboarding.DocumentRegistration (which writes the cache) and
-- Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate (which reads it).
--
-- RCOcrData is a LOCAL type (not from shared-kernel) so it carries its own JSON instances
-- independently of whatever version of Kernel.External.Verification.Interface.Types is pinned.
module SharedLogic.RCOcrCache
  ( RCOcrData (..),
    rcOcrCacheKey,
    cacheExtractedRC,
    getCachedExtractedRC,
  )
where

import qualified Domain.Types.Person as Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | All fields are Maybe so that the struct can be partially populated.
-- Currently only rcNumber is filled (from the Nix-pinned shared-kernel OCR response).
-- The remaining fields become non-null once shared-kernel is updated to a version
-- that returns full vehicle details from the OCR API.
data RCOcrData = RCOcrData
  { rcNumber :: Maybe Text,
    vehicleClass :: Maybe Text,
    manufacturer :: Maybe Text,
    model :: Maybe Text,
    fuelType :: Maybe Text,
    colour :: Maybe Text,
    chassisNumber :: Maybe Text,
    engineNumber :: Maybe Text,
    registrationDate :: Maybe Text,
    ownerName :: Maybe Text,
    manufacturingDate :: Maybe Text,
    bodyType :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

rcOcrCacheKey :: Id Person.Person -> Text
rcOcrCacheKey personId = "rcOcrData:" <> personId.getId

cacheExtractedRC ::
  ( MonadFlow m,
    Redis.HedisFlow m r,
    HasField "authTokenCacheExpiry" r Seconds
  ) =>
  Id Person.Person ->
  RCOcrData ->
  m ()
cacheExtractedRC personId ocrData = do
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  Redis.setExp (rcOcrCacheKey personId) ocrData authTokenCacheExpiry

getCachedExtractedRC ::
  ( MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Id Person.Person ->
  m (Maybe RCOcrData)
getCachedExtractedRC personId = Redis.get (rcOcrCacheKey personId)
