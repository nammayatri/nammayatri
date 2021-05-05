{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Context where

import Beckn.Types.App
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude

buildContext :: (L.MonadFlow m, MonadTime m) => Text -> Text -> Maybe BaseUrl -> Maybe BaseUrl -> m Context
buildContext action txnId bapUri bppUri = do
  currTime <- getCurrentTime
  msgId <- L.generateGUID
  return $
    Context
      { _domain = MOBILITY,
        _country = Just "IND",
        _city = Nothing,
        _action = action,
        _core_version = Just "0.8.2",
        _domain_version = Just "0.8.2",
        _bap_uri = bapUri,
        _bpp_uri = bppUri,
        _transaction_id = txnId,
        _message_id = msgId,
        _timestamp = currTime,
        _ttl = Nothing
      }

updateContext :: (L.MonadFlow m, MonadTime m) => Text -> Context -> m Context
updateContext action context = do
  currTime <- getCurrentTime
  msgId <- L.generateGUID
  return $
    context & #_timestamp .~ currTime
      & #_message_id .~ msgId
      & #_action .~ action