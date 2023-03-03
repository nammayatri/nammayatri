{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    getPersonDetails,
    updatePerson,
  )
where

import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runInReplica, runTransaction)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Text as TU
import SharedLogic.CallBPPInternal as CallBPPInternal
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson
import Storage.Tabular.Person ()
import Tools.Error

type ProfileRes = Person.PersonAPIEntity

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getPersonDetails :: (EsqDBReplicaFlow m r, EncFlow m r) => Id Person.Person -> m ProfileRes
getPersonDetails personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasBapInfo r m, CoreMetrics m) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email

  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode
  runTransaction $
    QPerson.updatePersonalInfo
      personId
      (req.firstName)
      (req.middleName)
      (req.lastName)
      refCode
      mbEncEmail
      (req.deviceToken)
      (req.language)
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasBapInfo r m, CoreMetrics m) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenRide) do
    throwError (InvalidRequest "You have already reffered someone")
  case person.referralCode of
    Just code ->
      if code /= refCode
        then throwError (InvalidRequest "Referral Code is not same")
        else return Nothing -- idempotent behaviour
    Nothing -> do
      merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          void $ CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId refCode mobileNumber countryCode
          return $ Just refCode
        _ -> throwError (InvalidRequest "Mobile number is null")
