module Domain.Action.UI.Aarokya where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time (utctDay)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.PartnerSdk.Interface.Types as PartnerSdk
import qualified Kernel.External.Verification.HyperVerge.Types as HV
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverIdentityInfo as DIInfo
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformationExtra as QDI
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QPan
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
    MonadFlow m,
    Redis.HedisLTSFlowEnv r
  ) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m AarokyaTokenRes
generateToken (personId, merchantId, merchantOpCityId) = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <-
    mapM decrypt person.mobileNumber
      >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  let countryCode = fromMaybe "" person.mobileCountryCode
  driverLicense <-
    QDL.findByDriverId personId
      >>= fromMaybeM (InvalidRequest "Driver license is required for Aarokya token generation")
  dlNumber <- decrypt driverLicense.licenseNumber
  -- Best-effort: a failure here (e.g. driver_identity_info not yet migrated in this
  -- environment, KV hiccup, unparseable Idfy data) must never fail token generation.
  -- withTryCatch logs and we fall back to no address.
  mbAddress <- either (const Nothing) (\x -> x) <$> withTryCatch "aarokya:resolveDriverAddress" (resolveDriverAddress personId merchantId merchantOpCityId person)
  -- DOB is the driver's own identity attribute, so it is read directly by driverId
  -- (no name match needed): Driving License first, PAN card as fallback. Optional.
  mbPanCard <- QPan.findByDriverId personId
  let mbDob = (T.pack . show . utctDay) <$> (driverLicense.driverDob <|> (mbPanCard >>= (.driverDob)))
  let mbLastName = case catMaybes [person.middleName, person.lastName] of
        [] -> Nothing
        parts -> Just (T.unwords parts)
  let mbGender = case show person.gender of
        g | g `elem` ["MALE", "FEMALE", "OTHER"] -> Just g
        _ -> Nothing
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

-- | Resolve the driver's address from their latest completed document
-- verification, trying each provider in turn. The fallback order is:
--   1. Idfy Driving License
--   2. Idfy Vehicle Registration Certificate (owner-name matched)
--   3. HyperVerge Driving License
--   4. HyperVerge Vehicle Registration Certificate (owner-name matched)
-- The DL address is preferred over the RC one (the RC owner is often a
-- relative / financier / fleet, so the RC address is only accepted when the
-- owner name matches the driver's). Always optional: returns Nothing on any
-- missing/unparseable data instead of failing the token call.
getDriverAddress ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  DP.Person ->
  m (Maybe Text)
getDriverAddress personId person =
  firstJustM
    [ getAddressFromDoc personId DVC.DriverLicense extractDlAddress,
      getAddressFromDoc personId DVC.VehicleRegistrationCertificate (extractRcAddress driverName),
      getAddressFromHVDoc personId DVC.DriverLicense extractHvDlAddress,
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

-- | Returns the driver's address, resolving it lazily: if one is already stored
-- (DriverIdentityInfo.address, falling back to the legacy DriverInformation.address)
-- it is reused as-is and no Idfy parsing happens. Only when nothing is stored do
-- we extract from the Idfy response and persist it for next time. Optional
-- throughout — never fails the token call.
resolveDriverAddress ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DP.Person ->
  m (Maybe Text)
resolveDriverAddress personId merchantId merchantOpCityId person = do
  mbDriverInfo <- QDI.findById (cast personId)
  mbIdentityInfo <- QDII.findByDriverId personId
  let mbStored = (mbIdentityInfo >>= (.address)) <|> (mbDriverInfo >>= (.address))
  case mbStored of
    Just stored -> pure (Just stored)
    Nothing -> do
      mbResolved <- getDriverAddress personId person
      whenJust ((,) <$> mbResolved <*> mbDriverInfo) $ \(address, driverInfo) ->
        Redis.withLockRedis (DIInfo.driverIdentityInfoLockKey personId) 10 $ do
          mbExisting <- QDII.findByDriverId personId
          void $
            DIInfo.upsertDriverIdentityInfo
              mbExisting
              personId
              merchantId
              merchantOpCityId
              driverInfo
              Nothing
              Nothing
              Nothing
              (Just address)
              Nothing
              Nothing
      pure mbResolved
