module Main (main) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Kernel.Prelude
import Network.HTTP.Types (Status, status200, status429)
import Network.Wai (Application, Request, defaultRequest, rawPathInfo, requestHeaders, requestMethod, responseHeaders, responseLBS, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import Utils.Common.RateLimit

main :: IO ()
main = hspec $ do
  pureSpec
  middlewareSpec

pureSpec :: Spec
pureSpec = do
  describe "clientIpFromXff" $ do
    it "takes the entry at the given position from the right" $
      clientIpFromXff 3 "1.1.1.1, 2.2.2.2, 3.3.3.3, 4.4.4.4" `shouldBe` Just "2.2.2.2"
    it "strips a port" $
      clientIpFromXff 1 "1.1.1.1:5555" `shouldBe` Just "1.1.1.1"
    it "returns Nothing when the header has too few entries" $
      clientIpFromXff 3 "1.1.1.1, 2.2.2.2" `shouldBe` Nothing
    it "ignores empty entries" $
      clientIpFromXff 2 "1.1.1.1,, 2.2.2.2" `shouldBe` Just "1.1.1.1"
  describe "ipMatchesEntry" $ do
    it "matches an exact address" $ ipMatchesEntry "10.1.2.3" "10.1.2.3" `shouldBe` True
    it "does not match a different address" $ ipMatchesEntry "10.1.2.4" "10.1.2.3" `shouldBe` False
    it "matches inside a CIDR" $ ipMatchesEntry "10.200.3.4" "10.0.0.0/8" `shouldBe` True
    it "does not match outside a CIDR" $ ipMatchesEntry "11.0.0.1" "10.0.0.0/8" `shouldBe` False
    it "handles /32" $ do
      ipMatchesEntry "1.2.3.4" "1.2.3.4/32" `shouldBe` True
      ipMatchesEntry "1.2.3.5" "1.2.3.4/32" `shouldBe` False
    it "handles /0" $ ipMatchesEntry "8.8.8.8" "0.0.0.0/0" `shouldBe` True
    it "rejects malformed entries" $ do
      ipMatchesEntry "1.2.3.4" "1.2.3.4/33" `shouldBe` False
      ipMatchesEntry "1.2.3.4" "1.2.3/24" `shouldBe` False
      ipMatchesEntry "1.2.3.4" "x/8" `shouldBe` False
      ipMatchesEntry "999.2.3.4" "999.0.0.0/8" `shouldBe` False
  describe "isExcepted" $ do
    let ex = RateLimitExceptions ["65.1.235.189", "10.0.0.0/8"] ["tok-raw", hashToken "tok-hashed"]
    it "exempts a listed ip" $ isExcepted ex (Just "65.1.235.189") Nothing `shouldBe` True
    it "exempts an ip inside a listed cidr" $ isExcepted ex (Just "10.9.9.9") Nothing `shouldBe` True
    it "exempts a listed raw token" $ isExcepted ex Nothing (Just "tok-raw") `shouldBe` True
    it "exempts a token listed by its sha256 hash" $ isExcepted ex Nothing (Just "tok-hashed") `shouldBe` True
    it "exempts when either the ip or the token is listed" $ isExcepted ex (Just "9.9.9.9") (Just "tok-raw") `shouldBe` True
    it "does not exempt unlisted callers" $ isExcepted ex (Just "9.9.9.9") (Just "other") `shouldBe` False
    it "does not exempt when nothing is known" $ isExcepted ex Nothing Nothing `shouldBe` False
  describe "window math" $ do
    it "buckets by window" $ do
      windowBucket 119 60 `shouldBe` 1
      windowBucket 120 60 `shouldBe` 2
    it "computes seconds until the window ends" $ do
      retryAfterSeconds 120 60 `shouldBe` 60
      retryAfterSeconds 179 60 `shouldBe` 1
  describe "compileRules" $ do
    let lim = Just (RateLimit 5 1)
        mk n pat i t = RateLimitRule n pat Nothing i t Nothing
    it "accepts a valid rule" $ do
      let (errs, ok) = compileRules [mk "a" ".*/x" lim Nothing]
      errs `shouldBe` []
      length ok `shouldBe` 1
    it "rejects a rule with neither limit" $ do
      let (errs, ok) = compileRules [mk "a" ".*/x" Nothing Nothing]
      length errs `shouldBe` 1
      length ok `shouldBe` 0
    it "rejects non-positive limits" $ do
      fst (compileRules [mk "a" ".*/x" (Just (RateLimit 0 1)) Nothing]) `shouldSatisfy` (not . null)
      fst (compileRules [mk "a" ".*/x" Nothing (Just (RateLimit 5 0))]) `shouldSatisfy` (not . null)
    it "rejects an invalid regex and keeps the valid rules" $ do
      let (errs, ok) = compileRules [mk "bad" "(" lim Nothing, mk "good" ".*/x" lim Nothing]
      length errs `shouldBe` 1
      map (.rule.name) ok `shouldBe` ["good"]
  describe "matchingRules" $ do
    let rules = snd (compileRules [RateLimitRule "list" ".*/ui/driver/ride/list.*" Nothing (Just (RateLimit 5 1)) Nothing Nothing, RateLimitRule "post-only" ".*/confirm" (Just ["POST"]) (Just (RateLimit 5 1)) Nothing Nothing])
    it "matches by path regex" $
      map (.rule.name) (matchingRules rules "GET" "/dobpp/ui/driver/ride/list") `shouldBe` ["list"]
    it "respects the method filter" $ do
      map (.rule.name) (matchingRules rules "GET" "/a/confirm") `shouldBe` []
      map (.rule.name) (matchingRules rules "POST" "/a/confirm") `shouldBe` ["post-only"]
    it "returns nothing for unrelated paths" $
      null (matchingRules rules "GET" "/health") `shouldBe` True

data Fixture = Fixture
  { counters :: IORef (Map.Map Text Integer),
    logs :: IORef [Text],
    configs :: IORef (Map.Map Text Text),
    failCounter :: IORef Bool
  }

newFixture :: [(Text, Text)] -> IO Fixture
newFixture cfgs = Fixture <$> newIORef Map.empty <*> newIORef [] <*> newIORef (Map.fromList cfgs) <*> newIORef False

backendOf :: Fixture -> RateLimitBackend
backendOf fx =
  RateLimitBackend
    { fetchConfig = \k -> Map.lookup k <$> readIORef fx.configs,
      incrWithExpire = \key _ -> do
        shouldFail <- readIORef fx.failCounter
        when shouldFail $ ioError (userError "redis down")
        atomicModifyIORef' fx.counters (\m -> let n = Map.findWithDefault 0 key m + 1 in (Map.insert key n m, n)),
      logInfoIO = \l -> modifyIORef' fx.logs (l :),
      logErrorIO = \l -> modifyIORef' fx.logs (l :),
      refreshSeconds = 3600,
      clientIpPositionFromRight = 1
    }

mkRequest :: ByteString -> Maybe ByteString -> Maybe ByteString -> Request
mkRequest path xff tok =
  defaultRequest
    { rawPathInfo = path,
      requestMethod = "GET",
      requestHeaders = maybe [] (\v -> [("x-forwarded-for", v)]) xff <> maybe [] (\v -> [("token", v)]) tok
    }

callWith :: (Application -> Application) -> Request -> IO (Status, Maybe ByteString)
callWith middleware req = do
  out <- newIORef (status200, Nothing)
  let inner _ respond = respond (responseLBS status200 [] "")
      respondTo res = writeOut out res
  _ <- middleware inner req respondTo
  readIORef out
  where
    writeOut out res = do
      modifyIORef' out (const (responseStatus res, lookup "Retry-After" (responseHeaders res)))
      pure ResponseReceived

rulesJson :: Text
rulesJson =
  "[{\"name\":\"ride_list\",\"pathPattern\":\".*/ui/driver/ride/list.*\",\"ip\":{\"maxRequests\":3,\"windowMinutes\":1},\"token\":{\"maxRequests\":2,\"windowMinutes\":1}}]"

middlewareSpec :: Spec
middlewareSpec = describe "rateLimitMiddleware" $ do
  let path = "/dobpp/ui/driver/ride/list"
      fixtureWithRules extra = newFixture ([("rate_limit_rules", rulesJson)] <> extra)
      runN n mw req = replicateM n (callWith mw req)

  it "allows everything when no rules are configured" $ do
    fx <- newFixture []
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- replicateM 10 (callWith mw (mkRequest path (Just "1.1.1.1") (Just "t")))
    map fst rs `shouldSatisfy` all (== status200)

  it "enforces the token limit with a 429 and Retry-After" $ do
    fx <- fixtureWithRules []
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 3 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    map fst rs `shouldBe` [status200, status200, status429]
    (snd (last rs) >>= readMaybe . T.unpack . decodeUtf8) `shouldSatisfy` maybe False (\(n :: Int) -> n >= 1 && n <= 60)

  it "enforces the ip limit independently of tokens" $ do
    fx <- fixtureWithRules []
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- forM [1 .. 4 :: Int] $ \i -> callWith mw (mkRequest path (Just "1.1.1.1") (Just ("tok" <> show i)))
    map fst rs `shouldBe` [status200, status200, status200, status429]

  it "counts different tokens and ips separately" $ do
    fx <- fixtureWithRules []
    mw <- rateLimitMiddleware (backendOf fx)
    a <- runN 2 mw (mkRequest path (Just "1.1.1.1") (Just "tokA"))
    b <- runN 2 mw (mkRequest path (Just "2.2.2.2") (Just "tokB"))
    map fst (a <> b) `shouldSatisfy` all (== status200)

  it "does not rate limit unrelated paths" $ do
    fx <- fixtureWithRules []
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 20 mw (mkRequest "/health" (Just "1.1.1.1") (Just "tok"))
    map fst rs `shouldSatisfy` all (== status200)

  it "applies no rule to an excepted ip and does not count it" $ do
    fx <- fixtureWithRules [("rate_limit_exceptions", "{\"ips\":[\"1.1.1.1\"],\"tokens\":[]}")]
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 20 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    map fst rs `shouldSatisfy` all (== status200)
    readIORef fx.counters >>= (`shouldSatisfy` Map.null)

  it "applies no rule to an excepted token even from an unlisted ip" $ do
    fx <- fixtureWithRules [("rate_limit_exceptions", "{\"tokens\":[\"vip\"]}")]
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 20 mw (mkRequest path (Just "5.5.5.5") (Just "vip"))
    map fst rs `shouldSatisfy` all (== status200)

  it "exempts a token listed by hash" $ do
    fx <- fixtureWithRules [("rate_limit_exceptions", "{\"tokens\":[\"" <> hashToken "vip2" <> "\"]}")]
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 20 mw (mkRequest path (Just "5.5.5.5") (Just "vip2"))
    map fst rs `shouldSatisfy` all (== status200)

  it "does not reject in shadow mode but logs the overage" $ do
    let shadowRules = "[{\"name\":\"s\",\"pathPattern\":\".*/ride/list.*\",\"token\":{\"maxRequests\":1,\"windowMinutes\":1},\"shadow\":true}]"
    fx <- newFixture [("rate_limit_rules", shadowRules)]
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 5 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    map fst rs `shouldSatisfy` all (== status200)
    logged <- readIORef fx.logs
    length (filter ("Rate limit exceeded" `T.isInfixOf`) logged) `shouldBe` 4

  it "never logs the raw token" $ do
    fx <- fixtureWithRules []
    mw <- rateLimitMiddleware (backendOf fx)
    _ <- runN 5 mw (mkRequest path Nothing (Just "super-secret-token"))
    logged <- readIORef fx.logs
    logged `shouldSatisfy` all (not . T.isInfixOf "super-secret-token")
    keys <- Map.keys <$> readIORef fx.counters
    keys `shouldSatisfy` all (not . T.isInfixOf "super-secret-token")

  it "fails open when the counter store is down" $ do
    fx <- fixtureWithRules []
    writeIORef' fx.failCounter True
    mw <- rateLimitMiddleware (backendOf fx)
    rs <- runN 10 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    map fst rs `shouldSatisfy` all (== status200)

  it "keeps the last good rules when a refresh returns invalid json" $ do
    fx <- fixtureWithRules []
    let backend = (backendOf fx) {refreshSeconds = 0}
    mw <- rateLimitMiddleware backend
    firstRound <- runN 3 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    map fst firstRound `shouldBe` [status200, status200, status429]
    modifyIORef' fx.configs (Map.insert "rate_limit_rules" "not json")
    afterChange <- callWith mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    fst afterChange `shouldBe` status429

  it "picks up a changed rule without any restart" $ do
    fx <- fixtureWithRules []
    let backend = (backendOf fx) {refreshSeconds = 0}
    mw <- rateLimitMiddleware backend
    firstRound <- runN 3 mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    fst (last firstRound) `shouldBe` status429
    modifyIORef' fx.configs (Map.delete "rate_limit_rules")
    afterChange <- callWith mw (mkRequest path (Just "1.1.1.1") (Just "tok"))
    fst afterChange `shouldBe` status200
  where
    writeIORef' ref v = modifyIORef' ref (const v)
