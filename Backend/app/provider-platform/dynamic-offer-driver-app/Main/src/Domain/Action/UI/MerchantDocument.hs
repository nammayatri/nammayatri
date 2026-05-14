module Domain.Action.UI.MerchantDocument
  ( getMerchantDocumentList,
    getMerchantDocument,
  )
where

import qualified API.Types.UI.MerchantDocument as API
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimit)
import qualified MerchantDocuments.Domain.Action.UI.MerchantDocument as SMD
import qualified MerchantDocuments.Domain.Types.MerchantDocument as DMD
import Storage.Beam.MerchantDocument ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

getMerchantDocumentList ::
  ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
    Id Domain.Types.Merchant.Merchant,
    Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Maybe Language ->
  DMD.Role ->
  Environment.Flow API.MerchantDocumentListRes
getMerchantDocumentList (_, merchantId, merchantOpCityId) mbLanguage role = do
  documents <-
    SMD.listMerchantDocuments
      (cast merchantId)
      (Kernel.Prelude.Just $ cast merchantOpCityId)
      role
      Kernel.Prelude.Nothing
      mbLanguage
  pure API.MerchantDocumentListRes {documents = documents}

merchantDocumentGetIpHitsCountKey :: Text -> Text
merchantDocumentGetIpHitsCountKey ip =
  "BPP:MerchantDocument:get:ip:" <> ip <> ":hitsCount"

extractClientIp :: Kernel.Prelude.Maybe Text -> Text
extractClientIp Kernel.Prelude.Nothing = "unknown"
extractClientIp (Kernel.Prelude.Just forwardedFor) =
  case T.splitOn "," forwardedFor of
    [] -> "unknown"
    (firstIp : _) -> T.strip firstIp

getMerchantDocument ::
  Text ->
  Kernel.Prelude.Maybe Language ->
  Kernel.Prelude.Maybe Context.City ->
  Id Domain.Types.Merchant.Merchant ->
  Kernel.Prelude.Maybe Text ->
  Environment.Flow DMD.MerchantDocument
getMerchantDocument documentType mbLanguage mbCity merchantId mbForwardedFor = do
  let clientIp = extractClientIp mbForwardedFor
  checkSlidingWindowLimit (merchantDocumentGetIpHitsCountKey clientIp)
  let language = Kernel.Prelude.fromMaybe ENGLISH mbLanguage
  mbMerchantOpCityId <-
    case mbCity of
      Kernel.Prelude.Nothing -> pure Kernel.Prelude.Nothing
      Kernel.Prelude.Just city -> do
        moc <-
          CQMOC.findByMerchantIdAndCity merchantId city
            >>= fromMaybeM (MerchantOperatingCityNotFound $ show city)
        pure $ Kernel.Prelude.Just moc.id
  SMD.getMerchantDocument
    (cast merchantId)
    (cast <$> mbMerchantOpCityId)
    documentType
    DMD.Driver
    language
