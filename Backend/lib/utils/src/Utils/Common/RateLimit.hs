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
    lookupClientIpPositionFromRight,
    rulesConfigId,
    exceptionsConfigId,
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
import Network.Wai (Middleware, Request, Response, rawPathInfo, requestHeaders, requestMethod, responseLBS)
import qualified System.Environment as Env
import Text.Regex.Base (makeRegexM, matchTest)
import Text.Regex.Posix (Regex)

rulesConfigId :: Text
rulesConfigId = "rate_limit_rules"

exceptionsConfigId :: Text
exceptionsConfigId = "rate_limit_exceptions"

data RateLimit = RateLimit
  { maxRequests :: Int,
    windowMinutes :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

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
    exceptions :: RateLimitExceptions
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

clientIpFromXff :: Int -> Text -> Maybe Text
clientIpFromXff positionFromRight header =
  let entries = filter (not . T.null) (map T.strip (T.splitOn "," header))
      idx = length entries - positionFromRight
   in if idx < 0
        then Nothing
        else
          listToMaybe (drop idx entries) >>= \entry ->
            let host = T.takeWhile (/= ':') entry
             in if T.null host then Nothing else Just host

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
      | any invalid (catMaybes [r.ip, r.token]) = Left ("rule " <> r.name <> " has non-positive maxRequests or windowMinutes")
      | otherwise = case makeRegexM (T.unpack r.pathPattern) of
        Nothing -> Left ("rule " <> r.name <> " has invalid pathPattern " <> r.pathPattern)
        Just re -> Right (CompiledRule r re)
    invalid l = l.maxRequests <= 0 || l.windowMinutes <= 0

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
  let fresh = CachedConfig now newRules newExceptions
  atomicModifyIORef' ref (const (fresh, ()))
  pure fresh
  where
    fetchOne cid = either (Left . show) Right <$> E.try @SomeException (backend.fetchConfig cid)

checkLimit :: RateLimitBackend -> Int -> CompiledRule -> Text -> Text -> RateLimit -> IO (Maybe Violation)
checkLimit backend now c dimension subject lim = do
  let windowSecs = lim.windowMinutes * 60
      key = "RateLimit:" <> c.rule.name <> ":" <> dimension <> ":" <> subject <> ":" <> show (windowBucket now windowSecs)
  result <- E.try @SomeException (backend.incrWithExpire key (retryAfterSeconds now windowSecs + 1))
  case result of
    Left err -> backend.logErrorIO ("rate limit counter failed, allowing request: " <> show err) >> pure Nothing
    Right count
      | count > fromIntegral lim.maxRequests -> do
        let isShadow = fromMaybe False c.rule.shadow
        backend.logInfoIO
          ( "Rate limit exceeded: rule=" <> c.rule.name <> " by=" <> dimension <> " subject=" <> subject
              <> " count="
              <> show count
              <> " limit="
              <> show lim.maxRequests
              <> "/"
              <> show lim.windowMinutes
              <> "m"
              <> " shadow="
              <> show isShadow
          )
        pure (Just (Violation (retryAfterSeconds now windowSecs) isShadow))
      | otherwise -> pure Nothing

evaluate :: RateLimitBackend -> CachedConfig -> Request -> IO (Maybe Int)
evaluate backend cfg req = do
  let path = decodeUtf8 (rawPathInfo req)
      meth = T.toUpper (decodeUtf8 (requestMethod req))
      headers = requestHeaders req
      clientIp = lookup "x-forwarded-for" headers >>= clientIpFromXff backend.clientIpPositionFromRight . decodeUtf8
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
  ref <- newIORef (CachedConfig 0 [] (RateLimitExceptions [] []))
  pure $ \inner req respond -> do
    cfg <- currentConfig backend ref
    verdict <- evaluate backend cfg req
    maybe (inner req respond) (respond . tooManyRequests) verdict
