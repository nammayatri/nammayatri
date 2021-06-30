{-# LANGUAGE PolyKinds #-}

module Beckn.Utils.Servant.API
  ( type (:>|),
    PlainText_ISO_8859_1,
  )
where

import Data.ByteString.Lazy (toStrict)
import Data.Text
import Data.Text.Encoding
import EulerHS.Prelude hiding (toStrict)
import qualified Network.HTTP.Media as M
import Servant

-- | This behaves similarly to ':>' from API point of view, but in implementation
-- it attaches a parameter to /each/ separate endpoint, not /all/ of them at once.
--
-- For instance:
-- @
-- type API = Header "auth" Text :> (Get '[JSON] Text :<|> Post '[JSON] ())
-- @
--
-- requires the following implementation:
--
-- @
-- handlers :: Server API
-- handlers = \auth -> get auth :<|> new auth
-- @
--
-- But when ':>' is replaced with ':>|', you can write just
--
-- @
-- handlers = get :<|> auth
-- @
--
-- Note that ':>|' has fewer priority that ':<|>' so you can omit parentheses.
--
-- This operator is experimental, if you find ':>' more appropriate then use it.
type family (:>|) (pre :: k) (api :: Type) where
  pre :>| (api1 :<|> api2) = (pre :>| api1) :<|> (pre :>| api2)
  pre :>| api = pre :> api

infixr 2 :>|

data PlainText_ISO_8859_1 deriving (Typeable)

instance Accept PlainText_ISO_8859_1 where
  contentType _ = "text" M.// "plain" M./: ("charset", "ISO-8859-1")

instance MimeUnrender PlainText_ISO_8859_1 Text where
  mimeUnrender _ = Right . decodeLatin1 . toStrict
