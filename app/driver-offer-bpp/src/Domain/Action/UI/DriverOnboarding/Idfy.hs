module Domain.Action.UI.DriverOnboarding.Idfy where

import Beckn.Prelude
import Beckn.Storage.Esqueleto (Transactionable (runTransaction))
import Beckn.Types.Error
import Beckn.Utils.Common
import qualified Data.Text as T
import Data.Time.Format
import Domain.Types.DriverOnboarding.ClassOfVehicle
import Environment
import qualified Idfy.Types.VerificationResult as Idfy
import Storage.Queries.DriverOnboarding.DriverLicense (updateDLDetails)
import Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate (updateRCDetails)

-- data IdfyDLReq = IdfyDLReq
--   { request_id :: Text,
--     status :: Text,
--     result :: DLVerificationResult
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- newtype DLVerificationResult = DLVerificationResult {source_output :: SourceOutput}
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- data SourceOutput = SourceOutput
--   { name :: Text,
--     field :: [CovDetails],
--     nt_validity_from :: Maybe Text,
--     nt_validity_to :: Maybe Text
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- newtype CovDetails = CovDetails {cov :: Text}
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- data IdfyRCReq = IdfyRCReq
--   { status :: Text,
--     request_id :: Text,
--     result :: RCVerificationResult
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- newtype RCVerificationResult = RCVerificationResult {extraction_output :: ExtractionOutput}
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

-- data ExtractionOutput = ExtractionOutput
--   { registration_date :: Maybe Text,
--     fitness_upto :: Maybe Text,
--     insurance_validity :: Maybe Text,
--     permit_validity :: Maybe Text,
--     vehicle_class :: Maybe Text,
--     registration_number :: Maybe Text
--   }
--   deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

idfyDL :: Idfy.DLVerificationResponse -> Flow AckResponse
idfyDL req = do
  now <- getCurrentTime
  let validFrom = getUTCTimeFromDate req.result.source_output.nt_validity_from
  let validTill = getUTCTimeFromDate req.result.source_output.nt_validity_to
  let cov = foldr f [] req.result.source_output.cov_details
  let verificationStatus = validateDLStatus validTill cov req.status now
  runTransaction $ updateDLDetails (Just req.request_id) validFrom validTill verificationStatus [LMV] now
  pure Ack
  where
    f = \x acc -> x.cov : acc

idfyRC :: Idfy.RCVerificationResponse -> Flow AckResponse
idfyRC reqs = do
  now <- getCurrentTime
  when (null reqs) $ throwError (InvalidRequest "RC_RESPONSE_EXPECTED")
  let req = head reqs
  let fitness_upto_ = getUTCTimeFromDate req.result.extraction_output.fitness_upto
  let insurance_validity_ = getUTCTimeFromDate req.result.extraction_output.insurance_validity
  let permit_validity_ = getUTCTimeFromDate req.result.extraction_output.permit_validity
  let registration_date_ = getUTCTimeFromDate req.result.extraction_output.registration_date
  let vehicle_class_ = req.result.extraction_output.vehicle_class
  let verificationStatus = validateRCStatus permit_validity_ insurance_validity_ vehicle_class_ req.status now
  runTransaction $
    updateRCDetails (Just req.request_id) registration_date_ permit_validity_ fitness_upto_ insurance_validity_ verificationStatus (Just LMV) now
  pure Ack

getUTCTimeFromDate :: Maybe Text -> Maybe UTCTime
getUTCTimeFromDate = \case
  Nothing -> Nothing
  Just date -> do
    parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack date

-- validateDL:
--     dl expiry
--     dl status
--     class of vehicle should be LMV or higher

--add validation for class of vehicle
validateDLStatus :: Maybe UTCTime -> [Idfy.ClassOfVehicle] -> Text -> UTCTime -> VerificationStatus
validateDLStatus validTill cov status now = case status of
  "COMPLETED" -> do
    let tr = idfyValidityStatus now validTill
    let cr = foldr' (\x acc -> isValidCOV x || acc) False cov
    if tr && cr then VALID else INVALID
  "FAILED" -> INVALID
  "IN_PROGRESS" -> PENDING
  _ -> PENDING

-- validate:
--         rc exp. -> using permit validity
--         rc status -> using status
--         insurance validity
--         vehicle class

--add validation for class of vehicle
validateRCStatus :: Maybe UTCTime -> Maybe UTCTime -> Maybe Idfy.ClassOfVehicle -> Text -> UTCTime -> VerificationStatus
validateRCStatus rcExpiry insuranceValidity cov status now = case status of
  "COMPLETED" -> do
    let tr = idfyValidityStatus now rcExpiry
    let cr = maybe False isValidCOV cov
    let ir = idfyValidityStatus now insuranceValidity
    if tr && ir && cr then VALID else INVALID
  "FAILED" -> INVALID
  "IN_PROGRESS" -> PENDING
  _ -> PENDING

idfyValidityStatus :: UTCTime -> Maybe UTCTime -> Bool
idfyValidityStatus now = maybe False (now <=)

inValidCov :: [Idfy.ClassOfVehicle]
inValidCov = [Idfy.W_NT, Idfy.W_T, Idfy.W_CAB, Idfy.MCWG, Idfy.MCWOG, Idfy.MGV, Idfy.MMV]

isValidCOV :: Idfy.ClassOfVehicle -> Bool
isValidCOV cov = foldr' (\x acc -> x /= cov && acc) True inValidCov
