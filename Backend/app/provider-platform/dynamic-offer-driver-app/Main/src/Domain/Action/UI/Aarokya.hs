module Domain.Action.UI.Aarokya where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Action.External.Aarokya as ExtAarokya
import Domain.Types.External.Aarokya (AarokyaTokenRequest (..))
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.Person as QP
import Tools.Metrics (CoreMetrics)
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
  aarokyaCfg <- getAarokyaSdkConfig merchantOpCityId
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <-
    mapM decrypt person.mobileNumber
      >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  let countryCode = fromMaybe "" person.mobileCountryCode
  mbDriverLicense <- QDL.findByDriverId personId
  mbDlNumber <- mapM (\dl -> decrypt dl.licenseNumber) mbDriverLicense
  let req =
        AarokyaTokenRequest
          { phone_country_code = countryCode,
            phone_number = mobileNumber,
            platform_id = aarokyaCfg.platformId,
            dl_number = mbDlNumber
          }
  res <- ExtAarokya.callAarokyaGenerateToken aarokyaCfg req
  let mbStatus = extractStatusFromToken res.access_token
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
      { token = res.access_token,
        personDetails = personDetails
      }

extractStatusFromToken :: Text -> Maybe Text
extractStatusFromToken token = do
  jwt <- JWT.decode token
  let claimsMap = JWT.unClaimsMap $ JWT.unregisteredClaims $ JWT.claims jwt
  case M.lookup "status" claimsMap of
    Just (A.String s) -> Just (T.toUpper s)
    _ -> Nothing

getAarokyaSdkConfig ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m ExtraMSC.AarokyaSdkConfig
getAarokyaSdkConfig merchantOpCityId = do
  msc <-
    CQMSC.findByServiceAndCity (ExtraMSC.InsuranceSdkService ExtraMSC.Aarokya) merchantOpCityId
      >>= fromMaybeM (InternalError $ "Aarokya SDK service config not found for merchantOpCityId: " <> merchantOpCityId.getId)
  case (msc :: DMSC.MerchantServiceConfig).serviceConfig of
    ExtraMSC.InsuranceSdkServiceConfig cfg -> pure cfg
    _ -> throwError $ InternalError "Unexpected service config shape for InsuranceSdkService Aarokya"
