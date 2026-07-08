module Domain.Action.UI.Aarokya where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (Day, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import qualified Kernel.External.Verification.HyperVerge.Types as HV
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.HyperVergeVerification as QHVV
import qualified Storage.Queries.IdfyVerificationExtra as QIV
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
    EncFlow m r,
    MonadFlow m
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
  -- Aadhaar is the preferred source for address / DOB / gender: it is the driver's
  -- own identity document, structured, and provider-agnostic. Only a VALID record is
  -- trusted; anything else is ignored and we fall back to the other sources.
  mbAadhaar <- QAadhaarCard.findByPrimaryKey personId
  let validAadhaar = mbAadhaar >>= \a -> if a.verificationStatus == Documents.VALID then Just a else Nothing
      mbAadhaarAddress = nonEmptyText =<< (validAadhaar >>= (.address))
  -- Best-effort: a failure here (e.g. driver_identity_info not yet migrated in this
  -- environment, KV hiccup, unparseable document data) must never fail token generation.
  -- withTryCatch logs and we fall back to no address.
  mbAddress <- either (const Nothing) (\addr -> addr) <$> withTryCatch "aarokya:getDriverAddress" (getDriverAddress personId person mbAadhaarAddress)
  -- DOB is the driver's own identity attribute, so it is read directly by driverId
  -- (no name match needed): Aadhaar first (raw text, parsed & normalised to ISO),
  -- then Driving License. All optional.
  let mbDob =
        (normalizeAadhaarDob =<< (validAadhaar >>= (.dateOfBirth)))
          <|> ((T.pack . show . utctDay) <$> driverLicense.driverDob)
  let mbLastName = case catMaybes [person.middleName, person.lastName] of
        [] -> Nothing
        parts -> Just (T.unwords parts)
  -- Gender: Aadhaar (normalised to MALE/FEMALE/OTHER) first, then person.gender.
  let currentGender = case show person.gender of
        g | g `elem` ["MALE", "FEMALE", "OTHER"] -> Just g
        _ -> Nothing
      mbGender = (normalizeGender =<< (validAadhaar >>= (.driverGender))) <|> currentGender
  let req =
        PartnerSdk.GenerateTokenReq
          { phoneCountryCode = countryCode,
            phoneNumber = mobileNumber,
            idProof = PartnerSdk.IdProof {proofType = "DRIVING_LICENSE", number = dlNumber},
            address = mbAddress,
            dob = mbDob,
            gender = mbGender,
            firstName = Just person.firstName,
            lastName = mbLastName
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

-- | Resolve the driver's address, trying each source in turn. The fallback order is:
--   1. Aadhaar (structured column, the driver's own address)
--   2. Idfy Driving License
--   3. HyperVerge Driving License
--   4. Idfy Vehicle Registration Certificate (owner-name matched)
--   5. HyperVerge Vehicle Registration Certificate (owner-name matched)
-- The DL addresses are preferred over the RC ones (the RC owner is often a
-- relative / financier / fleet, so an RC address is only accepted when the
-- owner name matches the driver's). Always optional: returns Nothing on any
-- missing/unparseable data instead of failing the token call.
getDriverAddress ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DP.Person ->
  Maybe Text ->
  m (Maybe Text)
getDriverAddress personId person mbAadhaarAddress =
  firstJustM
    [ pure mbAadhaarAddress,
      getAddressFromDoc personId DVC.DriverLicense extractDlAddress,
      getAddressFromHVDoc personId DVC.DriverLicense extractHvDlAddress,
      getAddressFromDoc personId DVC.VehicleRegistrationCertificate (extractRcAddress driverName),
      getAddressFromHVDoc personId DVC.VehicleRegistrationCertificate (extractHvRcAddress driverName)
    ]
  where
    driverName = T.unwords $ catMaybes [Just person.firstName, person.middleName, person.lastName]

-- | Runs the given lookups in order and returns the first that yields a result.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = pure Nothing
firstJustM (act : rest) = act >>= maybe (firstJustM rest) (pure . Just)

getAddressFromDoc ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  (Idfy.IdfyResult -> Maybe Text) ->
  m (Maybe Text)
getAddressFromDoc personId docType extract = do
  mbVer <- QIV.findLatestCompletedByDriverIdAndDocType personId docType
  pure $ do
    ver <- mbVer
    respText <- ver.idfyResponse
    Idfy.VerificationResponse rsp <- decodeFromText respText
    result <- rsp.result
    extract result

extractDlAddress :: Idfy.IdfyResult -> Maybe Text
extractDlAddress = \case
  Idfy.DLResult srcOp -> nonEmptyText =<< srcOp.source_output.address
  _ -> Nothing

extractRcAddress :: Text -> Idfy.IdfyResult -> Maybe Text
extractRcAddress driverName = \case
  Idfy.RCResult extOp -> do
    let rc = extOp.extraction_output
    ownerName <- rc.owner_name
    guard (namesMatch driverName ownerName)
    nonEmptyText =<< rc.permanent_address <|> (nonEmptyText =<< rc.current_address)
  _ -> Nothing

-- | HyperVerge counterpart of 'getAddressFromDoc'. The HyperVerge raw response
-- is stored unparsed in @hypervergeResponse@, so we re-decode it into
-- 'HV.GetVerificationStatusResp' and pull the document data out of the nested
-- @result.apiOutput.result@ payload. We scan the most recent verifications
-- (a failed retry has no parseable address and is skipped) and take the first
-- that yields an address.
getAddressFromHVDoc ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DVC.DocumentType ->
  (HV.VerificationResultData -> Maybe Text) ->
  m (Maybe Text)
getAddressFromHVDoc personId docType extract = do
  vers <- QHVV.findLatestByDriverIdAndDocType (Just 5) (Just 0) personId docType
  pure $ listToMaybe $ mapMaybe extractFromVer vers
  where
    extractFromVer ver = do
      respText <- ver.hypervergeResponse
      resp <- decodeFromText respText :: Maybe HV.GetVerificationStatusResp
      statusResult <- resp.result
      apiResult <- statusResult.apiOutput.result
      extract apiResult

extractHvDlAddress :: HV.VerificationResultData -> Maybe Text
extractHvDlAddress = \case
  HV.DLVerificationResultData dl -> nonEmptyText =<< dl.address
  _ -> Nothing

extractHvRcAddress :: Text -> HV.VerificationResultData -> Maybe Text
extractHvRcAddress driverName = \case
  HV.RCVerificationResultData rc -> do
    info <- rc.rcInfo
    ownerName <- info.owner_name
    guard (namesMatch driverName ownerName)
    (nonEmptyText =<< info.permanent_full_address) <|> (nonEmptyText =<< info.current_full_address)
  _ -> Nothing

-- | Lenient same-person check for the RC fallback. The RC owner is often a
-- relative / financier / fleet rather than the driver, so we only accept the
-- RC address when the names plausibly refer to the same person.
--
-- Done inline at request time: tokenise both names (uppercase, split on any
-- non-alphanumeric char, drop honorifics and single-letter initials) and check
-- that every token of the shorter name appears in the longer one. It is
-- order-independent, so it handles reordering (KUMARA H M / H M KUMARA) and
-- dropped/added tokens (SANDEEP / SANDEEP KUMAR), while still rejecting
-- genuinely different owners.
namesMatch :: Text -> Text -> Bool
namesMatch a b =
  let ta = nameTokens a
      tb = nameTokens b
   in not (null ta) && not (null tb) && (all (`elem` tb) ta || all (`elem` ta) tb)

nameTokens :: Text -> [Text]
nameTokens =
  filter (\t -> T.length t >= 2 && t `notElem` honorifics)
    . T.words
    . T.toUpper
    . T.map (\c -> if isAlphaNum c then c else ' ')
  where
    honorifics = ["MR", "MRS", "MS", "DR", "SMT", "SHRI", "KUM"]

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = if T.null (T.strip t) then Nothing else Just t

-- | Aadhaar DOB is stored as raw text whose format varies by source (DigiLocker /
-- OTP). Parse it against the known formats and re-emit it as ISO @YYYY-MM-DD@ (the
-- format the SDK expects). Returns Nothing if none of the formats parse.
normalizeAadhaarDob :: Text -> Maybe Text
normalizeAadhaarDob raw =
  let s = T.unpack (T.strip raw)
      tryFmt fmt = parseTimeM True defaultTimeLocale fmt s :: Maybe Day
   in T.pack . formatTime defaultTimeLocale "%Y-%m-%d"
        <$> foldr ((<|>) . tryFmt) Nothing ["%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%d%m%Y"]

-- | Normalise an Aadhaar gender string to the SDK's MALE/FEMALE/OTHER. Returns
-- Nothing on anything unrecognised so we fall back to person.gender.
normalizeGender :: Text -> Maybe Text
normalizeGender raw = case T.toUpper (T.strip raw) of
  g
    | g `elem` ["M", "MALE"] -> Just "MALE"
    | g `elem` ["F", "FEMALE"] -> Just "FEMALE"
    | g `elem` ["O", "OTHER", "TRANSGENDER", "T"] -> Just "OTHER"
    | otherwise -> Nothing
