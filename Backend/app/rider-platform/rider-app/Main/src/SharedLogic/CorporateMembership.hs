-- | Corporate shuttle: which company (pass organisation) a rider belongs to.
--
-- A company is a 'RegularPass' pass organisation with work-email domains. A rider belongs to it when their business
-- email is verified (the existing /auth/business-email OTP flow) and its domain is one of the organisation's domains.
-- Membership is derived on every call, never stored, so removing a domain from the organisation revokes access at once.
module SharedLogic.CorporateMembership
  ( emailDomain,
    normaliseEmailDomains,
    isCorporateOrg,
    findCorporateOrg,
    requireCorporateOrg,
  )
where

import qualified Data.Char as Char
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Types.PassOrganization as DPassOrganization
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DPerson
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.PassOrganization as QPassOrganization
import Tools.Error

-- | Lower-cased domain of an email, same parsing as the business-email domain in Domain.Action.UI.Search.
emailDomain :: Text -> Maybe Text
emailDomain email =
  let domain = T.toLower . T.drop 1 . T.strip . snd $ T.breakOn "@" email
   in if T.isInfixOf "@" email && not (T.null domain) then Just domain else Nothing

-- | Public mail providers: anyone can get an address there, so they never prove employment.
freeMailDomains :: [Text]
freeMailDomains =
  [ "gmail.com",
    "googlemail.com",
    "yahoo.com",
    "yahoo.co.in",
    "ymail.com",
    "rediffmail.com",
    "outlook.com",
    "hotmail.com",
    "live.com",
    "msn.com",
    "icloud.com",
    "me.com",
    "aol.com",
    "protonmail.com",
    "proton.me",
    "gmx.com",
    "mail.com",
    "yandex.com"
  ]

-- | Lower-case, strip, drop a leading @, dedupe, and reject anything that is not a plain company domain.
normaliseEmailDomains :: MonadFlow m => [Text] -> m [Text]
normaliseEmailDomains domains = do
  let normalised = nub . filter (not . T.null) $ map (T.dropWhile (== '@') . T.toLower . T.strip) domains
  forM_ normalised $ \domain -> do
    unless (validDomain domain) $ throwError $ InvalidRequest $ "Invalid email domain: " <> domain
    when (domain `elem` freeMailDomains) $
      throwError $ InvalidRequest $ "A public mail domain cannot identify a company: " <> domain
  pure normalised
  where
    validDomain d =
      T.isInfixOf "." d
        && T.all (\c -> Char.isAsciiLower c || Char.isDigit c || c == '.' || c == '-') d
        && not (any (\label -> T.null label || "-" `T.isPrefixOf` label || "-" `T.isSuffixOf` label) (T.splitOn "." d))

-- | A corporate organisation is a regular-pass organisation that has work-email domains.
isCorporateOrg :: DPassOrganization.PassOrganization -> Bool
isCorporateOrg org = org.passEnum == DPassType.RegularPass && not (null org.emailDomains)

-- | The rider's company and their work-email domain, when the business email is verified and its domain belongs to a
-- company in the rider's city.
findCorporateOrg ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  DPerson.Person ->
  m (Maybe (DPassOrganization.PassOrganization, Text))
findCorporateOrg person =
  verifiedWorkEmailDomain person >>= \case
    Nothing -> pure Nothing
    Just domain -> fmap (,domain) <$> findOrgByDomain person domain

-- | 'findCorporateOrg', or the reason the rider cannot use the corporate shuttle.
requireCorporateOrg ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) =>
  DPerson.Person ->
  m (DPassOrganization.PassOrganization, Text)
requireCorporateOrg person = do
  domain <- verifiedWorkEmailDomain person >>= fromMaybeM (InvalidRequest "Please verify your work email to continue")
  org <- findOrgByDomain person domain >>= fromMaybeM (CompanyNotOnboarded domain)
  pure (org, domain)

-- | Domain of the rider's business email, only once that email is verified.
verifiedWorkEmailDomain :: (MonadFlow m, EncFlow m r) => DPerson.Person -> m (Maybe Text)
verifiedWorkEmailDomain person
  | person.businessProfileVerified /= Just True = pure Nothing
  | otherwise = (>>= emailDomain) <$> mapM decrypt person.businessEmail

findOrgByDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPerson.Person ->
  Text ->
  m (Maybe DPassOrganization.PassOrganization)
findOrgByDomain person domain = do
  orgs <- QPassOrganization.findByMerchantOperatingCityIdAndPassEnum person.merchantOperatingCityId DPassType.RegularPass
  pure $ find (\org -> domain `elem` org.emailDomains) orgs
