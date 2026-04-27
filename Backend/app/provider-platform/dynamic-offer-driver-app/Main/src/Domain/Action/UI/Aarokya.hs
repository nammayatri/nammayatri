module Domain.Action.UI.Aarokya where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)
import qualified Tools.PartnerSdk as TPartnerSdk
import qualified Web.JWT as JWT

data AarokyaPersonDetails = AarokyaPersonDetails
  { firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    gender :: Text,
    identifierType :: Text,
    identifier :: Maybe Text,
    countryCode :: Text,
    phoneNumber :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

data AarokyaTokenRes = AarokyaTokenRes
  { token :: Text,
    personDetails :: Maybe AarokyaPersonDetails
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

generateToken ::
  ( CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m AarokyaTokenRes
generateToken (personId, _merchantId, merchantOpCityId) = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <-
    mapM decrypt person.mobileNumber
      >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  let countryCode = fromMaybe "" person.mobileCountryCode
  driverLicense <-
    QDL.findByDriverId personId
      >>= fromMaybeM (InvalidRequest "Driver license is required for Aarokya token generation")
  dlNumber <- decrypt driverLicense.licenseNumber
  let req =
        PartnerSdk.GenerateTokenReq
          { phoneCountryCode = countryCode,
            phoneNumber = mobileNumber,
            idProof = PartnerSdk.IdProof {proofType = "DRIVING_LICENSE", number = dlNumber}
          }
  res <- TPartnerSdk.generateToken merchantOpCityId req
  let mbStatus = extractStatusFromToken res.accessToken
  let personDetails =
        if mbStatus == Just "ONBOARDING"
          then
            Just $
              AarokyaPersonDetails
                { firstName = person.firstName,
                  middleName = person.middleName,
                  lastName = person.lastName,
                  gender = show person.gender,
                  identifierType = show person.identifierType,
                  identifier = person.identifier,
                  countryCode = countryCode,
                  phoneNumber = mobileNumber
                }
          else Nothing
  pure $
    AarokyaTokenRes
      { token = res.accessToken,
        personDetails = personDetails
      }

extractStatusFromToken :: Text -> Maybe Text
extractStatusFromToken token = do
  jwt <- JWT.decode token
  let claimsMap = JWT.unClaimsMap $ JWT.unregisteredClaims $ JWT.claims jwt
  case M.lookup "status" claimsMap of
    Just (A.String s) -> Just (T.toUpper s)
    _ -> Nothing
