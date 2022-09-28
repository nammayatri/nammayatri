{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import Beckn.External.Encryption (encrypt)
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Types.Validation (Validate)
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation (runRequestValidation, validateField)
import qualified Domain.Types.Person as Person
import Environment
import qualified Storage.Queries.DriverInformation as DriverInformation

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
  runTransaction $ DriverInformation.addReferralCode personId value
  return Success
