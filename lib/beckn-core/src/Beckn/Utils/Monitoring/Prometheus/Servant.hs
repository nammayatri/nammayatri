module Beckn.Utils.Monitoring.Prometheus.Servant where

import Data.Proxy
import Data.Text as DT
import EulerHS.Prelude as E
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.Wai (Request (..))
import Servant

class SanitizedUrl a where
  getSanitizedUrl :: Proxy a -> Request -> Maybe Text

instance
  (SanitizedUrl (a :: Type), SanitizedUrl (b :: Type)) =>
  SanitizedUrl (a :<|> b)
  where
  getSanitizedUrl _ req =
    getSanitizedUrl (Proxy :: Proxy a) req
      <|> getSanitizedUrl (Proxy :: Proxy b) req

instance
  ( KnownSymbol (path :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (path :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else do
        let (x : xs) = path
            p = DT.pack $ symbolVal (Proxy :: Proxy path)
        if p == x
          then
            let maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
             in (\url -> Just (p <> "/" <> url)) =<< maybeUrl
          else Nothing

instance
  ( KnownSymbol (capture :: Symbol),
    SanitizedUrl (subroute :: Type)
  ) =>
  SanitizedUrl (Capture capture a :> subroute)
  where
  getSanitizedUrl _ req = do
    let path = pathInfo req
    if E.null path
      then Nothing
      else
        let (_ : xs) = path
            p = DT.pack $ ":" <> symbolVal (Proxy :: Proxy capture)
            maybeUrl = getSanitizedUrl (Proxy :: Proxy subroute) $ req {pathInfo = xs}
         in (\url -> Just (p <> "/" <> url)) =<< maybeUrl

instance
  ReflectMethod m =>
  SanitizedUrl (Verb (m :: StdMethod) code contentType a)
  where
  getSanitizedUrl _ req = do
    let p = pathInfo req
    if E.null p && requestMethod req == reflectMethod (Proxy :: Proxy m)
      then Just ""
      else Nothing

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParams (h :: Symbol) a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header h a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (ReqBody cts a :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (QueryParam' modifier name t :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)

instance
  SanitizedUrl (subroute :: Type) =>
  SanitizedUrl (Header' '[Required, Strict] h v :> subroute)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy subroute)
