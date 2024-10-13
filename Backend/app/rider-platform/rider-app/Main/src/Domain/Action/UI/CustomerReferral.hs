{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.CustomerReferral where

import API.Types.UI.CustomerReferral
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Referral as Referral
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as PStats
import Tools.Auth
import Tools.Error

getCustomerRefferalCount :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Flow ReferredCustomers
getCustomerRefferalCount (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  stats <- PStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  pure $ ReferredCustomers {count = stats.referralCount}

postPersonApplyReferral :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ApplyCodeReq -> Flow ReferrerInfo
postPersonApplyReferral (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  res <- Referral.applyReferralCode person shouldShareReferrerInfo req.code
  let mbAndroidId = bool req.androidId Nothing (isJust person.androidId)
      mbDeviceId = bool req.deviceId Nothing (isJust person.deviceId)
  void $ QPerson.updateAndroidIdAndDeviceId personId mbAndroidId mbDeviceId
  case res of
    Left success ->
      -- NOTE: Error shouldn't come if driver app is released with the latest version & before that this api shouldn't be used.
      throwError . InternalError $ "Expected to have referrerInfo but got success: " <> show success
    Right referrerInfo -> pure referrerInfo
  where
    shouldShareReferrerInfo = True
