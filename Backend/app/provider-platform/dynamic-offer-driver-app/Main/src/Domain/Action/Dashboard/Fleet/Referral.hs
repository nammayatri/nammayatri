{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.Dashboard.Fleet.Referral where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import qualified Domain.Types.Person as DP
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation (Validate)
import Kernel.Utils.Common (fromMaybeM, throwError)
import Kernel.Utils.Validation (runRequestValidation, validateField)
import qualified Storage.Queries.DriverReferral as QDR
import Tools.Error

newtype FleetReferralReq = FleetReferralReq
  {value :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

newtype GetReferredFleetOwnerRes = GetReferredFleetOwnerRes
  {value :: Int}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

data FleetReferralRes = SuccessCode {val :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

instance ToJSON FleetReferralRes where
  toJSON (SuccessCode val) = A.object ["result" .= ("SuccessCode" :: Text), "val" .= val]

-- toJSON AlreadyReferred = A.object ["result" .= ("AlreadyReferred" :: Text)]
-- toJSON InvalidCode = A.object ["result" .= ("InvalidCode" :: Text)]

instance FromJSON FleetReferralRes where
  parseJSON (A.Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "SuccessCode" -> SuccessCode <$> obj .: "val"
      -- "AlreadyReferred" -> pure AlreadyReferred
      -- "InvalidCode" -> pure InvalidCode
      _ -> parseFail "Expected \"SuccessCode\""
  parseJSON err = typeMismatch "String" err

validateFleetReferralReq :: Validate FleetReferralReq
validateFleetReferralReq FleetReferralReq {..} =
  sequenceA_
    [ validateField "value" value $ ExactLength 6
    ]

isValidReferralForFleet ::
  FleetReferralReq ->
  Flow FleetReferralRes
isValidReferralForFleet req = do
  runRequestValidation validateFleetReferralReq req
  od <- B.runInReplica (QDR.findByRefferalCode $ Id req.value) >>= fromMaybeM (InvalidReferralCode req.value)
  if od.role == DP.OPERATOR
    then return $ SuccessCode od.driverId.getId
    else throwError $ InvalidReferralCode req.value
