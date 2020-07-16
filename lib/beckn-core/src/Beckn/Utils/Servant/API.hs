{-# LANGUAGE PolyKinds #-}

module Beckn.Utils.Servant.API
  ( type (:>|),
  )
where

import Data.Kind (Type)
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
