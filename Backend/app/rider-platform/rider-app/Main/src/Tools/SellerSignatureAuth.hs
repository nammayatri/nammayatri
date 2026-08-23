{-# LANGUAGE UndecidableInstances #-}

module Tools.SellerSignatureAuth (SellerSignatureAuth) where

import qualified Data.Aeson as A
import Data.List (lookup)
import Data.Maybe (listToMaybe)
import Data.Singletons.TH
import qualified Data.Text as T
import Environment (AppEnv)
import EulerHS.Prelude
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl (..))
import Kernel.Utils.Servant.Server (HasEnvEntry, getEnvEntry, runFlowRDelayedIO)
import qualified Kernel.Utils.Servant.SignatureAuth as SA
import Kernel.Utils.SignatureAuth (bodyHashHeader)
import qualified Network.Wai as Wai
import Servant hiding (throwError)
import Servant.Server.Internal.Delayed (addAuthCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, withRequest)
import qualified SharedLogic.FRFSSeller.Common as Common

data SellerSignatureAuth (domain :: Domain.Domain) (header :: Symbol)

instance
  ( HasServer api ctx,
    HasEnvEntry AppEnv ctx,
    KnownSymbol header,
    SingI domain
  ) =>
  HasServer (SellerSignatureAuth domain header :> api) ctx
  where
  type
    ServerT (SellerSignatureAuth domain header :> api) m =
      SA.SignatureAuthResult -> ServerT api m

  route _ ctx subserver =
    route (Proxy @api) ctx $
      subserver `addAuthCheck` withRequest authCheck'
    where
      authCheck' :: Wai.Request -> DelayedIO SA.SignatureAuthResult
      authCheck' req = runFlowRDelayedIO env . becknApiHandler . withLogTag "sellerAuthCheck" $ do
        let headers = Wai.requestHeaders req
            pathInfo = decodeUtf8 (Wai.rawPathInfo req)
        (operatorSlug, actionTxt) <-
          firstAndLastSegment pathInfo
            & fromMaybeM (InternalError $ "Seller beckn path has no operator segment: " <> show pathInfo)
        merchant <-
          Common.findSellerMerchant operatorSlug
            >>= fromMaybeM (MerchantDoesNotExist operatorSlug)
        action <- case A.fromJSON (A.String actionTxt) of
          A.Success a -> pure a
          A.Error err -> throwError (InternalError $ "Could not parse api name: " <> show actionTxt <> "; err: " <> show err)
        let subscriberType = case (headerName :: Text) of
              "X-Gateway-Authorization" -> Subscriber.BG
              _ -> Context.getSubscriberType action
            domain = fromSing (sing @domain)
        SA.authCheck
          headerName
          (lookup headerName headers)
          (lookup bodyHashHeader headers)
          merchant.id.getId
          subscriberType
          domain

      headerName :: IsString a => a
      headerName = fromString (symbolVal (Proxy @header))
      env = getEnvEntry ctx

  hoistServerWithContext _ ctxp hst serv =
    hoistServerWithContext (Proxy @api) ctxp hst . serv

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (SellerSignatureAuth domain h :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

firstAndLastSegment :: Text -> Maybe (Text, Text)
firstAndLastSegment path =
  let segs = filter (not . T.null) (T.splitOn "/" path)
   in (,) <$> listToMaybe segs <*> listToMaybe (reverse segs)
