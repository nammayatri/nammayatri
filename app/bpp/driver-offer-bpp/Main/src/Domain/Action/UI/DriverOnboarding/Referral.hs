{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption (encrypt)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Validation (Validate)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation (runRequestValidation, validateField)
import qualified Storage.CachedQueries.DriverInformation as DriverInformation

newtype ReferralReq = ReferralReq
  {value :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type ReferralRes = APISuccess

validateReferralReq :: Validate ReferralReq
validateReferralReq ReferralReq {..} =
  sequenceA_
    [ validateField "value" value P.mobileNumber
    ]

addReferral ::
  Id Person.Person ->
  ReferralReq ->
  Flow ReferralRes
addReferral personId req = do
  runRequestValidation validateReferralReq req
  value <- encrypt req.value
  DriverInformation.addReferralCode personId value
  return Success
