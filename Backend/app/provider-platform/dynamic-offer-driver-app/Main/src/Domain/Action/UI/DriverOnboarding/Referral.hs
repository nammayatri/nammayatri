{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation (Validate)
import Kernel.Utils.Common (fromMaybeM)
import Kernel.Utils.Error (throwError)
import Kernel.Utils.Validation (runRequestValidation, validateField)
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverReferral as QDR
import Tools.Error (DriverInformationError (..), DriverReferralError (..))

newtype ReferralReq = ReferralReq
  {value :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type ReferralRes = APISuccess

validateReferralReq :: Validate ReferralReq
validateReferralReq ReferralReq {..} =
  sequenceA_
    [ validateField "value" value $ ExactLength 6
    ]

addReferral ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ReferralReq ->
  Flow ReferralRes
addReferral (personId, _, _) req = do
  runRequestValidation validateReferralReq req
  di <- B.runInReplica (DriverInformation.findById personId) >>= fromMaybeM DriverInfoNotFound
  when (isJust di.referralCode || isJust di.referredByDriverId) $ throwError (AlreadyReffered personId.getId)
  dr <- B.runInReplica (QDR.findByRefferalCode $ Id req.value) >>= fromMaybeM (InvalidReferralCode req.value)
  DriverInformation.addReferralCode personId req.value dr.driverId
  return Success
