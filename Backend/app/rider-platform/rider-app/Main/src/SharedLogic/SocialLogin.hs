module SharedLogic.SocialLogin
  ( SocialLoginFlowM,
    VerifiedSocialLogin (..),
    verifySocialToken,
    toDomainProvider,
    toApiProvider,
    trustedEmail,
    hasTrustedEmail,
    normalizeEmail,
    isWellFormedEmail,
    findAccountByTrustedEmail,
    findIdentity,
    recordLogin,
    storePendingLink,
    storePendingEmailLink,
    linkPendingIdentity,
    linkIdentity,
    attachVerifiedEmail,
    unlinkProvider,
    deleteSocialIdentities,
    unlinkIdentitiesWithOtherEmail,
    enforceEmailOtpLimit,
  )
where

import qualified API.Types.UI.SocialLogin as SL
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantConfig as DMC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonSocialIdentity as DPSI
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (Encrypted, decrypt, encrypt)
import qualified Kernel.External.SocialLogin.Flow as SocialLogin
import Kernel.External.SocialLogin.Types (SocialLoginConfig, VerifiedSocialIdentity (..))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate (pFun)
import Kernel.Utils.Common
import qualified Kernel.Utils.EmailOtpRateLimit as EOL
import qualified Kernel.Utils.Predicates as P
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.PersonSocialIdentity as QPSI

type SocialLoginFlowM m r =
  ( SocialLogin.SocialLoginFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["socialLoginConfig" ::: SocialLoginConfig]
  )

data VerifiedSocialLogin = VerifiedSocialLogin
  { provider :: DPSI.SocialLoginProvider,
    socialIdentity :: VerifiedSocialIdentity,
    appleRefreshToken :: Maybe (Encrypted Text)
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data PendingLink = PendingLink
  { merchantId :: Id DM.Merchant,
    verifiedLogin :: Maybe VerifiedSocialLogin,
    verifiedEmail :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

defaultIdentityValidityInDays :: Int
defaultIdentityValidityInDays = 180

toDomainProvider :: SL.OAuthProvider -> DPSI.SocialLoginProvider
toDomainProvider SL.Google = DPSI.GOOGLE
toDomainProvider SL.IOS = DPSI.APPLE

toApiProvider :: DPSI.SocialLoginProvider -> SL.OAuthProvider
toApiProvider DPSI.GOOGLE = SL.Google
toApiProvider DPSI.APPLE = SL.IOS

toEmailSource :: VerifiedSocialLogin -> DP.EmailSource
toEmailSource verifiedLogin = case verifiedLogin.provider of
  DPSI.GOOGLE -> DP.SOCIAL_GOOGLE
  DPSI.APPLE
    | verifiedLogin.socialIdentity.isPrivateEmail -> DP.SOCIAL_APPLE_PRIVATE_RELAY
    | otherwise -> DP.SOCIAL_APPLE

verifySocialToken :: SocialLoginFlowM m r => SL.OAuthProvider -> Maybe Text -> Maybe Text -> Text -> m VerifiedSocialLogin
verifySocialToken oauthProvider mbNonce mbAuthorizationCode idToken = do
  cfg <- asks (.socialLoginConfig)
  case oauthProvider of
    SL.Google -> do
      socialIdentity <- SocialLogin.verifyGoogleIdToken cfg mbNonce idToken
      pure VerifiedSocialLogin {provider = DPSI.GOOGLE, socialIdentity, appleRefreshToken = Nothing}
    SL.IOS -> do
      socialIdentity <- SocialLogin.verifyAppleIdToken cfg mbNonce idToken
      appleRefreshToken <- maybe (pure Nothing) (exchangeAppleCode cfg socialIdentity.audience socialIdentity.subject) mbAuthorizationCode
      pure VerifiedSocialLogin {provider = DPSI.APPLE, socialIdentity, appleRefreshToken}

exchangeAppleCode :: SocialLoginFlowM m r => SocialLoginConfig -> Text -> Text -> Text -> m (Maybe (Encrypted Text))
exchangeAppleCode cfg clientId subject authorizationCode =
  withTryCatch "exchangeAppleAuthorizationCode" (SocialLogin.exchangeAppleAuthorizationCode cfg clientId subject authorizationCode) >>= \case
    Left err -> do
      logError $ "Apple authorization code exchange failed: " <> show err
      pure Nothing
    Right mbRefreshToken -> mapM encrypt mbRefreshToken

trustedEmail :: VerifiedSocialLogin -> Maybe Text
trustedEmail verifiedLogin =
  if verifiedLogin.socialIdentity.emailVerified && not verifiedLogin.socialIdentity.isPrivateEmail
    then verifiedLogin.socialIdentity.email
    else Nothing

hasTrustedEmail :: DP.Person -> Bool
hasTrustedEmail person = person.emailVerified == Just True || (person.identifierType == DP.EMAIL && isNothing person.emailVerified)

hasPrivateRelayEmail :: DP.Person -> Bool
hasPrivateRelayEmail person = person.emailSource == Just DP.SOCIAL_APPLE_PRIVATE_RELAY

acceptsNewEmail :: DP.Person -> Bool
acceptsNewEmail person = not (hasTrustedEmail person) || hasPrivateRelayEmail person

emailToAttach :: DP.Person -> VerifiedSocialLogin -> Maybe Text
emailToAttach person verifiedLogin
  | not verifiedLogin.socialIdentity.emailVerified = Nothing
  | hasPrivateRelayEmail person && verifiedLogin.socialIdentity.isPrivateEmail = Nothing
  | hasPrivateRelayEmail person = verifiedLogin.socialIdentity.email
  | verifiedLogin.socialIdentity.isPrivateEmail && hasTrustedEmail person = Nothing
  | person.emailVerified == Just True = Nothing
  | otherwise = verifiedLogin.socialIdentity.email

ensureEmailMatchesAccount :: SocialLoginFlowM m r => GenericError -> DP.Person -> Text -> m ()
ensureEmailMatchesAccount mismatchError person email =
  unless (acceptsNewEmail person) $ do
    accountEmail <- mapM decrypt person.email
    unless ((normalizeEmail <$> accountEmail) == Just (normalizeEmail email)) $ throwError mismatchError

validateLinkEmail :: SocialLoginFlowM m r => GenericError -> DP.Person -> VerifiedSocialLogin -> m ()
validateLinkEmail mismatchError person verifiedLogin = do
  when (verifiedLogin.socialIdentity.emailVerified && not verifiedLogin.socialIdentity.isPrivateEmail) $
    whenJust verifiedLogin.socialIdentity.email $ ensureEmailMatchesAccount mismatchError person
  whenJust (emailToAttach person verifiedLogin) $ ensureEmailAttachable person

validateLinkOtpEmail :: SocialLoginFlowM m r => DP.Person -> Text -> m ()
validateLinkOtpEmail person email = do
  ensureEmailMatchesAccount mobileNumberLinkedToOtherEmail person email
  when (acceptsNewEmail person) $ ensureEmailAttachable person email

normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

isWellFormedEmail :: Text -> Bool
isWellFormedEmail = pFun P.email . T.strip

enforceEmailOtpLimit :: (CacheFlow m r, MonadFlow m, EncFlow m r) => [DMC.MerchantConfig] -> Text -> m ()
enforceEmailOtpLimit merchantConfigs email =
  whenJust (listToMaybe merchantConfigs) $ \mc ->
    EOL.enforceEmailOtpLimit
      "Customer:EmailAuthCount:"
      [ EOL.EmailOtpLimitWindow mc.authEmailCountWindow1 mc.authEmailCountThreshold1,
        EOL.EmailOtpLimitWindow mc.authEmailCountWindow2 mc.authEmailCountThreshold2
      ]
      email

findPersonByEmail :: SocialLoginFlowM m r => Id DM.Merchant -> Text -> m (Maybe DP.Person)
findPersonByEmail merchantId email =
  QPerson.findByEmailAndMerchantId merchantId email >>= \case
    Just person -> pure (Just person)
    Nothing
      | normalizeEmail email /= email -> QPerson.findByEmailAndMerchantId merchantId (normalizeEmail email)
      | otherwise -> pure Nothing

findAccountByTrustedEmail :: SocialLoginFlowM m r => Id DM.Merchant -> Maybe Text -> m (Maybe DP.Person)
findAccountByTrustedEmail merchantId mbEmail =
  case mbEmail of
    Nothing -> pure Nothing
    Just email -> do
      mbPerson <- findPersonByEmail merchantId email
      pure $ mbPerson >>= \person -> if hasTrustedEmail person then Just person else Nothing

findIdentity :: SocialLoginFlowM m r => Id DM.Merchant -> VerifiedSocialLogin -> m (Maybe DPSI.PersonSocialIdentity)
findIdentity merchantId verifiedLogin = QPSI.findByMerchantIdAndProviderAndSubject verifiedLogin.socialIdentity.subject verifiedLogin.provider merchantId

recordLogin :: SocialLoginFlowM m r => DPSI.PersonSocialIdentity -> VerifiedSocialLogin -> m ()
recordLogin socialIdentity verifiedLogin = do
  now <- getCurrentTime
  QPSI.updateByPrimaryKey
    socialIdentity
      { DPSI.appleRefreshToken = verifiedLogin.appleRefreshToken <|> socialIdentity.appleRefreshToken,
        DPSI.lastLoginAt = Just now,
        DPSI.updatedAt = now
      }

pendingLinkKey :: Text -> Text
pendingLinkKey linkToken = "SocialLogin:PendingLink:" <> linkToken

storePending :: SocialLoginFlowM m r => PendingLink -> m Text
storePending pending = do
  linkToken <- generateGUID
  Redis.setExp (pendingLinkKey linkToken) pending 900
  pure linkToken

storePendingLink :: SocialLoginFlowM m r => Id DM.Merchant -> VerifiedSocialLogin -> m Text
storePendingLink merchantId verifiedLogin = storePending PendingLink {merchantId, verifiedLogin = Just verifiedLogin, verifiedEmail = Nothing}

storePendingEmailLink :: SocialLoginFlowM m r => Id DM.Merchant -> Text -> m Text
storePendingEmailLink merchantId email = storePending PendingLink {merchantId, verifiedLogin = Nothing, verifiedEmail = Just email}

linkPendingIdentity :: SocialLoginFlowM m r => DP.Person -> Text -> m ()
linkPendingIdentity person linkToken = do
  pending :: PendingLink <- Redis.safeGet (pendingLinkKey linkToken) >>= fromMaybeM SocialLinkTokenInvalid
  unless (pending.merchantId == person.merchantId) $ throwError SocialLinkTokenInvalid
  whenJust pending.verifiedLogin $ validateLinkEmail mobileNumberLinkedToOtherEmail person
  whenJust pending.verifiedEmail $ validateLinkOtpEmail person
  claimed <- Redis.setNxExpire (pendingLinkKey linkToken <> ":Claimed") 900 True
  unless claimed $ throwError SocialLinkTokenInvalid
  Redis.del (pendingLinkKey linkToken)
  whenJust pending.verifiedLogin $ \verifiedLogin -> void $ linkIdentity person verifiedLogin
  whenJust pending.verifiedEmail $ \email -> when (acceptsNewEmail person) $ attachVerifiedEmail person email DP.OTP

linkIdentity :: SocialLoginFlowM m r => DP.Person -> VerifiedSocialLogin -> m DPSI.PersonSocialIdentity
linkIdentity person verifiedLogin = do
  validateLinkEmail emailMismatchWithAccount person verifiedLogin
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  now <- getCurrentTime
  let validityInDays = fromMaybe defaultIdentityValidityInDays merchant.socialIdentityValidityInDays
      validTill = addUTCTime (fromIntegral (validityInDays * 86400)) now
      subject = verifiedLogin.socialIdentity.subject
  email <- mapM encrypt verifiedLogin.socialIdentity.email
  mbOwnRow <-
    QPSI.findByMerchantIdAndProviderAndSubject subject verifiedLogin.provider person.merchantId >>= \case
      Just existing
        | existing.personId == person.id -> pure (Just existing)
        | existing.validTill <= now -> do
          QPSI.deleteById existing.id
          pure Nothing
        | otherwise -> throwError (InvalidRequest "This account is already connected to a different profile. Please sign in with it directly or use another account.")
      Nothing -> pure Nothing
  socialIdentity <- case mbOwnRow of
    Just existing -> do
      let relinked =
            existing
              { DPSI.issuer = verifiedLogin.socialIdentity.issuer,
                DPSI.clientId = verifiedLogin.socialIdentity.audience,
                DPSI.email = email <|> existing.email,
                DPSI.isPrivateEmail = verifiedLogin.socialIdentity.isPrivateEmail,
                DPSI.appleRefreshToken = verifiedLogin.appleRefreshToken <|> existing.appleRefreshToken,
                DPSI.lastLoginAt = Just now,
                DPSI.validTill = validTill,
                DPSI.updatedAt = now
              }
      QPSI.updateByPrimaryKey relinked
      pure relinked
    Nothing -> do
      QPSI.findByPersonIdAndProvider person.id verifiedLogin.provider >>= \case
        Just previous -> do
          QPSI.deleteByPersonIdAndProvider person.id verifiedLogin.provider
          revokeAppleTokens [previous]
        Nothing -> pure ()
      newId <- generateGUID
      let newIdentity =
            DPSI.PersonSocialIdentity
              { id = newId,
                personId = person.id,
                merchantId = person.merchantId,
                provider = verifiedLogin.provider,
                issuer = verifiedLogin.socialIdentity.issuer,
                providerSubject = subject,
                clientId = verifiedLogin.socialIdentity.audience,
                email,
                isPrivateEmail = verifiedLogin.socialIdentity.isPrivateEmail,
                appleRefreshToken = verifiedLogin.appleRefreshToken,
                lastLoginAt = Just now,
                validTill,
                createdAt = now,
                updatedAt = now
              }
      QPSI.create newIdentity
      pure newIdentity
  whenJust (emailToAttach person verifiedLogin) $ \verifiedEmail -> attachVerifiedEmail person verifiedEmail (toEmailSource verifiedLogin)
  pure socialIdentity

attachVerifiedEmail :: SocialLoginFlowM m r => DP.Person -> Text -> DP.EmailSource -> m ()
attachVerifiedEmail person email emailSource = do
  mbHolder <- findPersonByEmail person.merchantId email
  whenJust (mbHolder >>= \holder -> if holder.id /= person.id then Just holder else Nothing) $ \holder -> do
    when (holder.identifierType == DP.EMAIL) $ throwError emailRegisteredWithAnotherAccount
    QPersonExtra.clearEmail holder.id
  encEmail <- encrypt (normalizeEmail email)
  QPersonExtra.updateVerifiedEmail person.id encEmail emailSource

ensureEmailAttachable :: SocialLoginFlowM m r => DP.Person -> Text -> m ()
ensureEmailAttachable person email = do
  mbHolder <- findPersonByEmail person.merchantId email
  whenJust mbHolder $ \holder ->
    when (holder.id /= person.id && holder.identifierType == DP.EMAIL) $ throwError emailRegisteredWithAnotherAccount

emailRegisteredWithAnotherAccount :: GenericError
emailRegisteredWithAnotherAccount = InvalidRequest "This email is already in use on another profile. Please sign in using this email instead."

emailMismatchWithAccount :: GenericError
emailMismatchWithAccount = InvalidRequest "Your profile is registered with a different email. Please continue with the same email you used before."

mobileNumberLinkedToOtherEmail :: GenericError
mobileNumberLinkedToOtherEmail = InvalidRequest "This mobile number is already registered with a different email. Sign in with that email, or use a different mobile number."

unlinkProvider :: SocialLoginFlowM m r => Id DP.Person -> DPSI.SocialLoginProvider -> m ()
unlinkProvider personId provider =
  QPSI.findByPersonIdAndProvider personId provider >>= \case
    Just socialIdentity -> do
      QPSI.deleteByPersonIdAndProvider personId provider
      revokeAppleTokens [socialIdentity]
      when (provider == DPSI.APPLE) $
        QPerson.findById personId >>= \case
          Just person | hasPrivateRelayEmail person -> QPersonExtra.clearEmail personId
          _ -> pure ()
    Nothing -> throwError (InvalidRequest "This account is not connected to your profile.")

deleteSocialIdentities :: SocialLoginFlowM m r => Id DP.Person -> m ()
deleteSocialIdentities personId = do
  identities <- QPSI.findAllByPersonId personId
  unless (null identities) $ do
    QPSI.deleteAllByPersonId personId
    revokeAppleTokens identities

unlinkIdentitiesWithOtherEmail :: SocialLoginFlowM m r => Id DP.Person -> Maybe Text -> m ()
unlinkIdentitiesWithOtherEmail personId mbNewEmail = do
  identities <- QPSI.findAllByPersonId personId
  conflicting <- filterM hasOtherEmail identities
  unless (null conflicting) $ do
    forM_ conflicting $ \socialIdentity -> QPSI.deleteById socialIdentity.id
    revokeAppleTokens conflicting
  where
    hasOtherEmail socialIdentity = do
      mbIdentityEmail <- mapM decrypt socialIdentity.email
      pure $ case mbIdentityEmail of
        Nothing -> False
        Just identityEmail -> Just (normalizeEmail identityEmail) /= (normalizeEmail <$> mbNewEmail)

revokeAppleTokens :: SocialLoginFlowM m r => [DPSI.PersonSocialIdentity] -> m ()
revokeAppleTokens identities = do
  let tokens = [(socialIdentity.clientId, token) | socialIdentity <- identities, socialIdentity.provider == DPSI.APPLE, Just token <- [socialIdentity.appleRefreshToken]]
  unless (null tokens) $ do
    cfg <- asks (.socialLoginConfig)
    fork "revokeAppleRefreshTokens" $
      forM_ tokens $ \(clientId, encryptedToken) -> do
        refreshToken <- decrypt encryptedToken
        withTryCatch "revokeAppleRefreshToken" (SocialLogin.revokeAppleRefreshToken cfg clientId refreshToken) >>= \case
          Left err -> logError $ "Apple refresh token revoke failed: " <> show err
          Right () -> pure ()
