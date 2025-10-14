module Domain.Action.UI.Tokenization where

import qualified API.Types.UI.Tokenization
import qualified Data.Text as T
import Data.Time.Format
import qualified Data.Time.LocalTime as LT
import qualified Domain.Types.DriverGullakAssociation as Domain.Types.DriverGullakAssociation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DomainMSC
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Tokenize as Tokenize
import qualified Kernel.External.Tokenize.Interface.Types as TokenizeIntTypes
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMSOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.Queries.DriverGullakAssociation as QDGA
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.Person as SQP
import Tools.Error

getDriverSdkToken ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Int ->
    Tokenize.TokenizationService ->
    Environment.Flow API.Types.UI.Tokenization.GetTokenRes
  )
getDriverSdkToken (mbPersonId, merchantId, merchantOperatingCityId) expirySec svc = do
  svcfg <- (CQMSC.findByServiceAndCity (DomainMSC.TokenizationService svc) merchantOperatingCityId >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Tokenization" (show svc))) <&> (.serviceConfig)
  hvsc <- case svcfg of
    DomainMSC.TokenizationServiceConfig sc -> return sc
    _ -> throwError $ ServiceConfigError "Service Config is not Tokenization service config !!!!"
  case hvsc of
    Tokenize.GullakTokenizationServiceConfig _ -> case mbPersonId of
      Nothing -> throwError $ InternalError "PersonId is required for Gullak Tokenization"
      Just personId -> makeGullakTokenizeCall personId hvsc
    _ -> makeResponse <$> Tokenize.tokenize hvsc (makeTokenizeReq expirySec)
  where
    makeTokenizeReq :: Int -> Tokenize.TokenizationReq
    makeTokenizeReq expiry = Tokenize.TokenizationReq {expiry = Just expiry, code = Nothing, codeVerifier = Nothing}

    makeResponse :: Tokenize.TokenizationResp -> API.Types.UI.Tokenization.GetTokenRes
    makeResponse Tokenize.TokenizationResp {..} = API.Types.UI.Tokenization.GetTokenRes {expiry = Nothing, ..}

    makeGullakTokenizeCall personId hvsc = do
      gullakDriverAssc <- QDGA.findByPrimaryKey personId
      now <- getCurrentTime
      case gullakDriverAssc of
        Just gda -> do
          if gda.tokenExpiry > now
            then pure $ API.Types.UI.Tokenization.GetTokenRes {expiry = Just $ gda.tokenExpiry, token = gda.gullakToken}
            else do
              let loginReq = TokenizeIntTypes.LoginReq {merchantUserId = personId.getId}
              loginResp <- Tokenize.login hvsc loginReq
              let tokenizeResp = mkGullakResponseTokenizeRes loginResp now
              QDGA.updateGullakToken tokenizeResp.token (fromMaybe now tokenizeResp.expiry) personId
              pure tokenizeResp
        Nothing -> do
          onboardingReq <- makeGullakOnboardingTokenizeReq personId
          onBoardingResp <- Tokenize.onboard hvsc onboardingReq
          QDGA.create $ buildGullakDriverAssociation personId onBoardingResp now
          pure $ mkGullakResponseTokenizeRes onBoardingResp now

    buildGullakDriverAssociation personId onBoardingResp now =
      Domain.Types.DriverGullakAssociation.DriverGullakAssociation
        { driverId = personId,
          gullakToken = onBoardingResp.loginToken.accessToken,
          tokenExpiry = fromMaybe now $ parseTimestamp onBoardingResp.loginToken.expiryDate,
          merchantOperatingCityId = merchantOperatingCityId,
          merchantId = merchantId,
          createdAt = now,
          updatedAt = now
        }

    makeGullakOnboardingTokenizeReq personId = do
      person <- SQP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      unencryptedMobileNumber <- mapM decrypt person.mobileNumber
      dl <- QDL.findByDriverId personId
      merchantOperatingCity <- CQMSOC.findById merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
      let cityLatLon = merchantOperatingCity.location
      return $
        TokenizeIntTypes.OnboardingReq
          { merchantUserId = personId.getId,
            mobile = fromMaybe "" unencryptedMobileNumber,
            email = person.email,
            dob = T.pack . formatTime defaultTimeLocale "%d/%m/%Y" <$> (dl >>= (.driverDob)),
            pan = Nothing,
            name = Just $ T.intercalate "" (catMaybes [Just person.firstName, addSpace person.middleName, addSpace person.lastName]),
            isKycVerified = Nothing,
            address = Nothing,
            languageDetails =
              Just $
                TokenizeIntTypes.LanguageDetails
                  { defaultLanguage = fromMaybe "ENGLISH" (person.language <&> show),
                    languagesKnown = maybe ["ENGLISH"] (map show) person.languagesSpoken
                  },
            locationDetails =
              Just $
                TokenizeIntTypes.LocationDetails
                  { sparseCoordinates = [cityLatLon.lat, cityLatLon.lon],
                    state = show $ merchantOperatingCity.state,
                    city = show $ merchantOperatingCity.city
                  }
          }

    mkGullakResponseTokenizeRes resp _now =
      API.Types.UI.Tokenization.GetTokenRes
        { expiry = parseTimestamp resp.loginToken.expiryDate,
          token = resp.loginToken.accessToken
        }

    addSpace :: Maybe Text -> Maybe Text
    addSpace nameSection = (" " <>) <$> nameSection

parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp timeStamp = LT.zonedTimeToUTC <$> (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" (T.unpack timeStamp) :: Maybe LT.ZonedTime)
