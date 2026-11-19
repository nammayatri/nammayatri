module Utils.Common.RateLimit
  ( RateLimit (..),
    RateLimitRule (..),
    RateLimitExceptions (..),
    RateLimitBackend (..),
    CompiledRule (..),
    rateLimitMiddleware,
    compileRules,
    matchingRules,
    isExcepted,
    clientIpFromXff,
    ipMatchesEntry,
    hashToken,
    windowBucket,
    retryAfterSeconds,
    limitWindowSeconds,
    normalizedPath,
    lookupClientIpPositionFromRight,
    rulesConfigId,
    exceptionsConfigId,
    trustedProxiesConfigId,
  )
where

import qualified Control.Exception as E
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (eitherDecodeStrict, withObject, (.!=), (.:?))
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import Kernel.Prelude
import Network.HTTP.Types (status429)
import Network.Wai (Middleware, Request, Response, pathInfo, requestHeaders, requestMethod, responseLBS)
import qualified System.Environment as Env
import System.Timeout (timeout)
import Text.Regex.Base (makeRegexM, matchTest)
import Text.Regex.Posix (Regex)

rulesConfigId :: Text
rulesConfigId = "rate_limit_rules"

exceptionsConfigId :: Text
exceptionsConfigId = "rate_limit_exceptions"

trustedProxiesConfigId :: Text
trustedProxiesConfigId = "rate_limit_trusted_proxies"

data RateLimit = RateLimit
  { maxRequests :: Int,
    windowSeconds :: Maybe Int,
    windowMinutes :: Maybe Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

limitWindowSeconds :: RateLimit -> Int
limitWindowSeconds l = case (l.windowSeconds, l.windowMinutes) of
  (Just s, _) -> s
  (Nothing, Just m) -> m * 60
  _ -> 0

data RateLimitRule = RateLimitRule
  { name :: Text,
    pathPattern :: Text,
    methods :: Maybe [Text],
    ip :: Maybe RateLimit,
    token :: Maybe RateLimit,
    shadow :: Maybe Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data RateLimitExceptions = RateLimitExceptions
  { ips :: [Text],
    tokens :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON RateLimitExceptions where
  parseJSON = withObject "RateLimitExceptions" $ \o ->
    RateLimitExceptions <$> o .:? "ips" .!= [] <*> o .:? "tokens" .!= []

data CompiledRule = CompiledRule
  { rule :: RateLimitRule,
    regex :: Regex
  }

data RateLimitBackend = RateLimitBackend
  { fetchConfig :: Text -> IO (Maybe Text),
    incrWithExpire :: Text -> Int -> IO Integer,
    logInfoIO :: Text -> IO (),
    logErrorIO :: Text -> IO (),
    refreshSeconds :: Int,
    clientIpPositionFromRight :: Int
  }

data CachedConfig = CachedConfig
  { fetchedAt :: Int,
    compiled :: [CompiledRule],
    exceptions :: RateLimitExceptions,
    trustedProxies :: [Text]
  }

data Violation = Violation
  { retryAfter :: Int,
    shadowOnly :: Bool
  }

lookupClientIpPositionFromRight :: IO Int
lookupClientIpPositionFromRight = fromMaybe 3 . (>>= readMaybe) <$> Env.lookupEnv "XFF_CLIENT_IP_POSITION_FROM_RIGHT"

nowSeconds :: IO Int
nowSeconds = floor <$> getPOSIXTime

windowBucket :: Int -> Int -> Int
windowBucket now windowSecs = now `div` windowSecs

retryAfterSeconds :: Int -> Int -> Int
retryAfterSeconds now windowSecs = windowSecs - (now `mod` windowSecs)

hashToken :: Text -> Text
hashToken t = show (hash (encodeUtf8 t :: ByteString) :: Digest SHA256)

xffHost :: Text -> Maybe Text
xffHost raw =
  let entry = T.strip raw
      host
        | T.count ":" entry == 1 = T.takeWhile (/= ':') entry
        | otherwise = T.dropAround (`elem` ['[', ']']) entry
   in if T.null host then Nothing else Just host

clientIpFromXff :: Int -> [Text] -> Text -> Maybe Text
clientIpFromXff positionFromRight trusted header =
  let entries = mapMaybe xffHost (T.splitOn "," header)
      remaining = take (length entries - max 0 (positionFromRight - 1)) entries
      isTrusted addr = any (ipMatchesEntry addr) trusted
   in case reverse (filter (not . isTrusted) remaining) of
        (client : _) -> Just client
        [] -> listToMaybe remaining

normalizedPath :: Request -> Text
normalizedPath req = "/" <> T.intercalate "/" (filter (\seg -> not (T.null seg) && seg /= ".") (pathInfo req))

parseIPv4 :: Text -> Maybe Word32
parseIPv4 t = case T.splitOn "." t of
  [a, b, c, d] -> do
    os <- traverse octet [a, b, c, d]
    pure (foldl' (\acc o -> (acc `shiftL` 8) .|. o) 0 os)
  _ -> Nothing
  where
    octet s = do
      n <- readMaybe (T.unpack s) :: Maybe Int
      if n >= 0 && n <= 255 then Just (fromIntegral n :: Word32) else Nothing

ipMatchesEntry :: Text -> Text -> Bool
ipMatchesEntry addr entry =
  let (base, rest) = T.breakOn "/" entry
   in if T.null rest
        then addr == entry
        else fromMaybe False $ do
          prefixLen <- readMaybe (T.unpack (T.drop 1 rest)) :: Maybe Int
          guard (prefixLen >= 0 && prefixLen <= 32)
          baseW <- parseIPv4 base
          addrW <- parseIPv4 addr
          let netMask = if prefixLen == 0 then 0 else complement 0 `shiftL` (32 - prefixLen) :: Word32
          pure ((addrW .&. netMask) == (baseW .&. netMask))

isExcepted :: RateLimitExceptions -> Maybe Text -> Maybe Text -> Bool
isExcepted ex clientIp tokenValue = ipExcepted || tokenExcepted
  where
    ipExcepted = maybe False (\addr -> any (ipMatchesEntry addr) ex.ips) clientIp
    tokenExcepted = maybe False (\tk -> let h = hashToken tk in any (\e -> e == tk || T.toLower e == h) ex.tokens) tokenValue

compileRules :: [RateLimitRule] -> ([Text], [CompiledRule])
compileRules = foldr step ([], [])
  where
    step r (errs, ok) = case validate r of
      Left err -> (err : errs, ok)
      Right c -> (errs, c : ok)
    validate r
      | isNothing r.ip && isNothing r.token = Left ("rule " <> r.name <> " defines neither ip nor token limit")
      | any invalid (catMaybes [r.ip, r.token]) = Left ("rule " <> r.name <> " has non-positive maxRequests or window")
      | otherwise = case makeRegexM (T.unpack r.pathPattern) of
        Nothing -> Left ("rule " <> r.name <> " has invalid pathPattern " <> r.pathPattern)
        Just re -> Right (CompiledRule r re)
    invalid l = l.maxRequests <= 0 || limitWindowSeconds l <= 0

matchingRules :: [CompiledRule] -> Text -> Text -> [CompiledRule]
matchingRules rules meth path = filter matches rules
  where
    matches c =
      matchTest c.regex (T.unpack path)
        && maybe True (any (\m -> T.toUpper m == meth)) c.rule.methods

currentConfig :: RateLimitBackend -> IORef CachedConfig -> IO CachedConfig
currentConfig backend ref = do
  now <- nowSeconds
  cached <- readIORef ref
  if now - cached.fetchedAt < backend.refreshSeconds
    then pure cached
    else do
      claimed <- atomicModifyIORef' ref $ \c ->
        if now - c.fetchedAt < backend.refreshSeconds then (c, False) else (c {fetchedAt = now}, True)
      if claimed then refreshConfig backend ref now cached else readIORef ref

refreshConfig :: RateLimitBackend -> IORef CachedConfig -> Int -> CachedConfig -> IO CachedConfig
refreshConfig backend ref now previous = do
  rulesRes <- fetchOne rulesConfigId
  excRes <- fetchOne exceptionsConfigId
  proxyRes <- fetchOne trustedProxiesConfigId
  newRules <- case rulesRes of
    Left err -> backend.logErrorIO ("rate limit rules fetch failed: " <> err) >> pure previous.compiled
    Right Nothing -> pure []
    Right (Just txt) -> case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> backend.logErrorIO ("rate limit rules parse failed: " <> T.pack err) >> pure previous.compiled
      Right (rs :: [RateLimitRule]) -> do
        let (errs, ok) = compileRules rs
        forM_ errs $ \e -> backend.logErrorIO ("rate limit rule ignored: " <> e)
        pure ok
  newExceptions <- case excRes of
    Left err -> backend.logErrorIO ("rate limit exceptions fetch failed: " <> err) >> pure previous.exceptions
    Right Nothing -> pure (RateLimitExceptions [] [])
    Right (Just txt) -> case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> backend.logErrorIO ("rate limit exceptions parse failed: " <> T.pack err) >> pure previous.exceptions
      Right ex -> pure ex
  newProxies <- case proxyRes of
    Left err -> backend.logErrorIO ("rate limit trusted proxies fetch failed: " <> err) >> pure previous.trustedProxies
    Right Nothing -> pure []
    Right (Just txt) -> case eitherDecodeStrict (encodeUtf8 txt) of
      Left err -> backend.logErrorIO ("rate limit trusted proxies parse failed: " <> T.pack err) >> pure previous.trustedProxies
      Right (ps :: [Text]) -> pure ps
  let fresh = CachedConfig now newRules newExceptions newProxies
  atomicModifyIORef' ref (const (fresh, ()))
  pure fresh
  where
    fetchOne cid = do
      res <- E.try @SomeException (timeout 2000000 (backend.fetchConfig cid))
      pure $ case res of
        Left err -> Left (show err)
        Right Nothing -> Left "timed out"
        Right (Just v) -> Right v

checkLimit :: RateLimitBackend -> Int -> CompiledRule -> Text -> Text -> RateLimit -> IO (Maybe Violation)
checkLimit backend now c dimension subject lim = do
  let windowSecs = limitWindowSeconds lim
      key = "RateLimit:" <> c.rule.name <> ":" <> dimension <> ":" <> subject <> ":" <> show windowSecs <> ":" <> show (windowBucket now windowSecs)
  result <- E.try @SomeException (timeout 200000 (backend.incrWithExpire key (retryAfterSeconds now windowSecs + 1)))
  case result of
    Left err -> backend.logErrorIO ("rate limit counter failed, allowing request: " <> show err) >> pure Nothing
    Right Nothing -> backend.logErrorIO "rate limit counter timed out, allowing request" >> pure Nothing
    Right (Just count)
      | count > fromIntegral lim.maxRequests -> do
        let isShadow = fromMaybe False c.rule.shadow
            over = count - fromIntegral lim.maxRequests
        when (over == 1 || over `mod` 100 == 0) $
          backend.logInfoIO
            ( "Rate limit exceeded: rule=" <> c.rule.name <> " by=" <> dimension <> " subject=" <> subject
                <> " count="
                <> show count
                <> " limit="
                <> show lim.maxRequests
                <> "/"
                <> show windowSecs
                <> "s"
                <> " shadow="
                <> show isShadow
            )
        pure (Just (Violation (retryAfterSeconds now windowSecs) isShadow))
      | otherwise -> pure Nothing

evaluate :: RateLimitBackend -> CachedConfig -> Request -> IO (Maybe Int)
evaluate backend cfg req = do
  let path = normalizedPath req
      meth = T.toUpper (decodeUtf8 (requestMethod req))
      headers = requestHeaders req
      clientIp = lookup "x-forwarded-for" headers >>= clientIpFromXff backend.clientIpPositionFromRight cfg.trustedProxies . decodeUtf8
      tokenValue = decodeUtf8 <$> lookup "token" headers
      applicable = matchingRules cfg.compiled meth path
  if null applicable || isExcepted cfg.exceptions clientIp tokenValue
    then pure Nothing
    else do
      now <- nowSeconds
      violations <- fmap concat . forM applicable $ \c -> do
        ipRes <- forM ((,) <$> c.rule.ip <*> clientIp) $ \(lim, addr) -> checkLimit backend now c "ip" addr lim
        tokenRes <- forM ((,) <$> c.rule.token <*> tokenValue) $ \(lim, tk) -> checkLimit backend now c "token" (hashToken tk) lim
        pure (catMaybes [join ipRes, join tokenRes])
      let enforced = filter (not . (.shadowOnly)) violations
      pure $ if null enforced then Nothing else Just (maximum (map (.retryAfter) enforced))

tooManyRequests :: Int -> Response
tooManyRequests retryAfterSecs =
  responseLBS
    status429
    [("Content-Type", "application/json"), ("Retry-After", encodeUtf8 (show retryAfterSecs :: Text))]
    "{\"errorCode\":\"RATE_LIMIT_EXCEEDED\",\"errorMessage\":\"Too many requests, please retry later\"}"

rateLimitMiddleware :: RateLimitBackend -> IO Middleware
rateLimitMiddleware backend = do
  ref <- newIORef (CachedConfig 0 [] (RateLimitExceptions [] []) [])
  pure $ \inner req respond -> do
    cfg <- currentConfig backend ref
    verdict <- evaluate backend cfg req
    maybe (inner req respond) (respond . tooManyRequests) verdict
