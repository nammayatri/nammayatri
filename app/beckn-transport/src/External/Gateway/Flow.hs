{-# LANGUAGE TypeApplications #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Call
import Beckn.Types.API.Callback
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant (err500, errBody)

onSearch :: OnSearchReq -> Flow AckResponse
onSearch req@CallbackReq {context} = do
  env <- ask
  res <- case xGatewaySelector env of
    Just "NSDL" -> do
      let mNsdlBaseUrl = xGatewayNsdlUrl env
      case mNsdlBaseUrl of
        Just nsdlBaseUrl -> do
          callAPIWithTrail nsdlBaseUrl (API.nsdlOnSearch (nsdlUsername env) (nsdlPassword env) req) "on_search"
        Nothing -> L.throwException $ err500 {errBody = "invalid nsdl gateway url"}
    Just "JUSPAY" -> do
      let url = xGatewayUri env
      callAPIWithTrail url (API.onSearch (xGatewayApiKey env ?: "mobility-provider-key") req) "on_search"
    _ -> L.throwException $ err500 {errBody = "gateway not configured"}
  whenRight res $ \_ ->
    L.logInfo @Text "OnSearch" "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onSearch Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onTrackTrip :: OnTrackTripReq -> Flow AckResponse
onTrackTrip req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onTrackTrip req) "on_track"
  whenRight res $ \_ ->
    L.logInfo @Text "OnTrackTrip" "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending OnTrackTrip Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onUpdate :: OnUpdateReq -> Flow AckResponse
onUpdate req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onUpdate req) "on_update"
  whenRight res $ \_ ->
    L.logInfo @Text "OnUpdate" "OnUpdate callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending OnUpdate Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onConfirm :: OnConfirmReq -> Flow AckResponse
onConfirm req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onConfirm req) "on_confirm"
  whenRight res $ \_ ->
    L.logInfo @Text "OnConfirm" "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onConfirm Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onCancel :: OnCancelReq -> Flow AckResponse
onCancel req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onCancel req) "on_cancel"
  whenRight res $ \_ ->
    L.logInfo @Text "OnCancel" "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onCancel Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onStatus :: OnStatusReq -> Flow AckResponse
onStatus req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onStatus req) "on_status"
  whenRight res $ \_ ->
    L.logInfo @Text "OnStatus" "OnStatus callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onStatus Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

initiateCall :: CallReq -> Flow AckResponse
initiateCall req@CallReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  whenRight res $ \_ ->
    L.logInfo @Text "initiateCall" "initiateCall successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending initiateCall: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing
