{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Auth where

import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (sortBy)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (getDbHash)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonExtra as QPerson
import qualified Storage.Queries.RegistrationTokenExtra as QRegToken
import Tools.Auth (verifyPerson)

data InternalResp = InternalResp
  { riderId :: Id DP.Person,
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data CustomerAuthTokenResp = CustomerAuthTokenResp
  { token :: Text,
    personId :: Text,
    firstName :: Maybe Text,
    lastName :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

internalAuth :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe RegToken -> Maybe Text -> Flow InternalResp
internalAuth token apiKey = do
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  (riderId, currentMerchantId) <- verifyPerson (fromMaybe "" token)
  person <- QP.findById riderId >>= fromMaybeM (PersonNotFound riderId.getId)
  let merchantOpCityId = person.merchantOperatingCityId
  pure $
    InternalResp
      { riderId,
        merchantId = currentMerchantId,
        merchantOperatingCityId = merchantOpCityId
      }

getCustomerAuthToken ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text]
  ) =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe (Id Merchant) ->
  m CustomerAuthTokenResp
getCustomerAuthToken apiKey mbMobileNumber mbMobileCountryCode mbMerchantId = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (apiKey == Just internalAPIKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  mobileNumber <- mbMobileNumber & fromMaybeM (InvalidRequest "mobileNumber is required")
  mobileCountryCode <- mbMobileCountryCode & fromMaybeM (InvalidRequest "mobileCountryCode is required")
  merchantId <- mbMerchantId & fromMaybeM (InvalidRequest "merchantId is required")
  mobileNumberHash <- getDbHash mobileNumber
  person <- B.runInReplica (QPerson.findByMobileNumberAndMerchantId mobileCountryCode mobileNumberHash merchantId) >>= fromMaybeM (PersonDoesNotExist "Person not found for given mobile number")
  regTokens <- B.runInReplica $ QRegToken.findAllByPersonId person.id
  let verifiedTokens = filter (.verified) regTokens
  latestToken <- listToMaybe (sortBy (comparing (Down . (.updatedAt))) verifiedTokens) & fromMaybeM (InvalidRequest "No verified auth token found for this user")
  pure $
    CustomerAuthTokenResp
      { token = latestToken.token,
        personId = getId person.id,
        firstName = person.firstName,
        lastName = person.lastName
      }
