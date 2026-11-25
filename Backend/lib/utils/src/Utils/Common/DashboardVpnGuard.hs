module Utils.Common.DashboardVpnGuard
  ( DashboardVpnGuardConfig (..),
    DashboardVpnGuardBackend (..),
    Decision (..),
    configId,
    disabledConfig,
    parseConfig,
    dashboardVpnGuardMiddleware,
    decide,
    isGuardedPath,
    isExemptPath,
    clientIpOf,
    socketHost,
  )
where

import qualified Control.Exception as E
import Data.Aeson (eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Kernel.Prelude
import Network.HTTP.Types (Status, status403, status503)
import Network.Wai (Middleware, Request, Response, pathInfo, remoteHost, requestHeaders, responseLBS)
import System.Timeout (timeout)
import Utils.Common.RateLimit (clientIpFromXff, hashToken, ipMatchesEntry)

configId :: Text
configId = "dashboard_vpn_guard"

data DashboardVpnGuardConfig = DashboardVpnGuardConfig
  { enabled :: Bool,
    whitelistedRoles :: [Text],
    vpnIps :: [Text],
    trustedProxies :: [Text],
    roleCacheTtlSeconds :: Int,
    exemptPaths :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON DashboardVpnGuardConfig where
  parseJSON = withObject "DashboardVpnGuardConfig" $ \o ->
    DashboardVpnGuardConfig
      <$> o .:? "enabled" .!= False
      <*> o .:? "whitelistedRoles" .!= []
      <*> o .:? "vpnIps" .!= []
      <*> o .:? "trustedProxies" .!= []
      <*> o .:? "roleCacheTtlSeconds" .!= 0
      <*> o .:? "exemptPaths" .!= []

disabledConfig :: DashboardVpnGuardConfig
disabledConfig = DashboardVpnGuardConfig False [] [] [] 0 []

parseConfig :: Text -> Either Text DashboardVpnGuardConfig
parseConfig = either (Left . T.pack) Right . eitherDecodeStrict . encodeUtf8

data DashboardVpnGuardBackend = DashboardVpnGuardBackend
  { fetchConfig :: Text -> IO (Maybe Text),
    lookupRoleName :: Text -> IO (Maybe Text),
    logInfoIO :: Text -> IO (),
    logErrorIO :: Text -> IO (),
    clientIpPositionFromRight :: Int,
    refreshSeconds :: Int
  }

data Decision
  = Allow
  | DenyOutsideVpn Text (Maybe Text)
  | DenyLookupFailed
  deriving (Show, Eq)

data GuardState = GuardState
  { fetchedAt :: Int,
    config :: Maybe DashboardVpnGuardConfig
  }

segments :: Request -> [Text]
segments = filter (\s -> not (T.null s) && s /= ".") . pathInfo

isGuardedPath :: [Text] -> Bool
isGuardedPath ("direct-dashboard" : _) = True
isGuardedPath _ = False

isExemptPath :: [Text] -> [Text] -> Bool
isExemptPath patterns ("direct-dashboard" : rest) = any (matches . filter (not . T.null) . T.splitOn "/") patterns
  where
    matches pat = not (null pat) && length pat == length rest && and (zipWith (\p s -> p == "*" || p == s) pat rest)
isExemptPath _ _ = False

socketHost :: Text -> Maybe Text
socketHost raw =
  let host
        | "[" `T.isPrefixOf` raw = T.takeWhile (/= ']') (T.drop 1 raw)
        | otherwise = T.takeWhile (/= ':') raw
   in if T.null host then Nothing else Just host

clientIpOf :: Int -> [Text] -> Request -> Maybe Text
clientIpOf positionFromRight trusted req =
  case lookup "x-forwarded-for" (requestHeaders req) of
    Just xff -> clientIpFromXff positionFromRight trusted (decodeUtf8 xff)
    Nothing -> socketHost (show (remoteHost req))

decide ::
  DashboardVpnGuardConfig ->
  [Text] ->
  Maybe Text ->
  Maybe Text ->
  IO (Either () (Maybe Text)) ->
  IO Decision
decide cfg segs mbToken clientIp lookupRole
  | not cfg.enabled || not (isGuardedPath segs) || isExemptPath cfg.exemptPaths segs = pure Allow
  | isNothing mbToken = pure Allow
  | otherwise =
    lookupRole <&> \case
      Left () -> DenyLookupFailed
      Right Nothing -> Allow
      Right (Just role)
        | role `elem` cfg.whitelistedRoles -> Allow
        | maybe False (\ip -> any (ipMatchesEntry ip) cfg.vpnIps) clientIp -> Allow
        | otherwise -> DenyOutsideVpn role clientIp

nowSeconds :: IO Int
nowSeconds = floor <$> getPOSIXTime

trySync :: IO a -> IO (Either E.SomeException a)
trySync action =
  E.try action >>= \case
    Left e | Just (_ :: E.SomeAsyncException) <- E.fromException e -> E.throwIO e
    res -> pure res

loadConfig :: DashboardVpnGuardBackend -> Maybe DashboardVpnGuardConfig -> IO (Maybe DashboardVpnGuardConfig)
loadConfig backend previous =
  trySync (timeout 2000000 (backend.fetchConfig configId)) >>= \case
    Left e -> backend.logErrorIO ("dashboard vpn guard config fetch failed: " <> show e) >> pure previous
    Right Nothing -> backend.logErrorIO "dashboard vpn guard config fetch timed out" >> pure previous
    Right (Just Nothing) -> pure (Just disabledConfig)
    Right (Just (Just txt)) -> case parseConfig txt of
      Left e -> backend.logErrorIO ("dashboard vpn guard config parse failed: " <> e) >> pure previous
      Right cfg -> pure (Just cfg)

currentConfig :: DashboardVpnGuardBackend -> IORef GuardState -> IO (Maybe DashboardVpnGuardConfig)
currentConfig backend ref = do
  now <- nowSeconds
  st <- readIORef ref
  if now - st.fetchedAt < backend.refreshSeconds
    then pure st.config
    else do
      claimed <- atomicModifyIORef' ref $ \s ->
        if now - s.fetchedAt < backend.refreshSeconds then (s, False) else (s {fetchedAt = now}, True)
      if claimed
        then do
          cfg <- loadConfig backend st.config
          atomicModifyIORef' ref $ \s -> (s {config = cfg}, ())
          pure cfg
        else pure st.config

cachedLookup :: DashboardVpnGuardBackend -> Int -> IORef (Map.Map Text (Int, Maybe Text)) -> Text -> IO (Either () (Maybe Text))
cachedLookup backend ttl ref token = do
  now <- nowSeconds
  let key = hashToken token
  cached <- atomicModifyIORef' ref $ \m -> (m, Map.lookup key m)
  case cached of
    Just (expiresAt, role) | ttl > 0 && expiresAt > now -> pure (Right role)
    _ ->
      trySync (backend.lookupRoleName token) >>= \case
        Left e -> do
          backend.logErrorIO ("dashboard vpn guard role lookup failed: " <> show e)
          pure (Left ())
        Right role -> do
          when (ttl > 0 && isJust role) $
            atomicModifyIORef' ref $ \m ->
              let live = if Map.size m >= 10000 then Map.filter ((> now) . fst) m else m
               in (Map.insert key (now + ttl, role) live, ())
          pure (Right role)

errorResponse :: Status -> LByteString -> LByteString -> Response
errorResponse status code msg =
  responseLBS status [("Content-Type", "application/json")] ("{\"errorCode\":\"" <> code <> "\",\"errorMessage\":\"" <> msg <> "\"}")

dashboardVpnGuardMiddleware :: DashboardVpnGuardBackend -> IO Middleware
dashboardVpnGuardMiddleware backend = do
  now <- nowSeconds
  initial <- loadConfig backend Nothing
  stateRef <- newIORef (GuardState now initial)
  cacheRef <- newIORef Map.empty
  let checkFailed = errorResponse status503 "ACCESS_CHECK_FAILED" "Could not verify access, please retry."
  pure $ \inner req respond -> do
    let segs = segments req
    case decodeUtf8 <$> lookup "token" (requestHeaders req) of
      Just token
        | isGuardedPath segs ->
          currentConfig backend stateRef >>= \case
            Nothing -> respond checkFailed
            Just cfg -> do
              let clientIp = clientIpOf backend.clientIpPositionFromRight cfg.trustedProxies req
              verdict <- decide cfg segs (Just token) clientIp (cachedLookup backend cfg.roleCacheTtlSeconds cacheRef token)
              case verdict of
                Allow -> inner req respond
                DenyOutsideVpn role ip -> do
                  backend.logInfoIO $
                    "dashboard vpn guard: blocked role=" <> role
                      <> " ip="
                      <> fromMaybe "unknown" ip
                      <> " path=/"
                      <> T.intercalate "/" segs
                  respond (errorResponse status403 "ACCESS_DENIED_OUTSIDE_VPN" "This API can only be used over the VPN.")
                DenyLookupFailed -> respond checkFailed
      _ -> inner req respond
