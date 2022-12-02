{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Servant.MultipartFormData
  ( module Servant.Multipart,
  )
where

import Beckn.Prelude
import Beckn.Utils.Monitoring.Prometheus.Servant
import Servant hiding (ResponseHeader (..))
import Servant.Multipart
import qualified Servant.OpenApi as S

instance
  ( S.HasOpenApi api
  ) =>
  S.HasOpenApi (MultipartForm tag a :> api)
  where
  toOpenApi _ = S.toOpenApi (Proxy @api) -- TODO: implementing OpenAPI interpretation for Multipart.

instance
  SanitizedUrl (sub :: Type) =>
  SanitizedUrl (MultipartForm tag a :> sub)
  where
  getSanitizedUrl _ = getSanitizedUrl (Proxy :: Proxy sub)
