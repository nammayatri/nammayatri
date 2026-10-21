module Domain.Action.UI.LiwipStore (getLiwipSession) where

import qualified Data.Aeson as A
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DP
import qualified EulerHS.Types as Euler
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Servant (Header, JSON, Post, ReqBody, (:>))
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.Person as QP
import Tools.Error ()
import Tools.Metrics (CoreMetrics)

data LiwipSessionReq = LiwipSessionReq
  { externalUserId :: Text,
    name :: Text,
    phone :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type LiwipSessionAPI =
  "partner"
    :> "auth"
    :> "session"
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] LiwipSessionReq
    :> Post '[JSON] A.Value

liwipSessionAPI :: Proxy LiwipSessionAPI
liwipSessionAPI = Proxy

getLiwipSession ::
  (CoreMetrics m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  (Id DP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m A.Value
getLiwipSession (personId, _merchantId, merchantOpCityId) = do
  msc <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.LiwipStoreService ExtraMSC.LiwipStore)}) Nothing
      >>= fromMaybeM (InvalidRequest "Liwip store is not available in this city")
  cfg <- case msc.serviceConfig of
    DMSC.LiwipStoreServiceConfig c -> pure c
    _ -> throwError $ InternalError "Unexpected service config shape for LiwipStoreService"
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (InvalidRequest "Person is not linked with any mobile number")
  apiKey <- decrypt cfg.apiKey
  let req =
        LiwipSessionReq
          { externalUserId = personId.getId,
            name = T.unwords $ catMaybes [Just person.firstName, person.middleName, person.lastName],
            phone = T.takeEnd 10 $ T.filter isDigit mobileNumber
          }
  callAPI cfg.url (Euler.client liwipSessionAPI (Just apiKey) req) "liwipSession" liwipSessionAPI
    >>= fromEitherM (\err -> ExternalAPICallError (Just "LIWIP_SESSION_API_ERROR") cfg.url err)
