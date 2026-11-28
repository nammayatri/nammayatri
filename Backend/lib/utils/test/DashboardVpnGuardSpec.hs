module DashboardVpnGuardSpec (spec) where

import qualified Control.Exception as E
import Control.Monad (replicateM_)
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Kernel.Prelude
import Network.HTTP.Types (Status, status200, status403, status503)
import Network.Wai (Application, Request, defaultRequest, pathInfo, requestHeaders, responseLBS, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Test.Hspec
import Utils.Common.DashboardVpnGuard

loginPaths :: [Text]
loginPaths = ["user/login", "user/verify2FaSetup", "*/*/fleet/v2/login/otp", "*/*/ticketdashboard/login/verify", "CAC/auth"]

cfg :: DashboardVpnGuardConfig
cfg =
  DashboardVpnGuardConfig
    { enabled = True,
      whitelistedRoles = ["SUPER_ADMIN"],
      vpnIps = ["10.20.0.0/16", "52.1.2.3"],
      trustedProxies = ["10.0.0.0/8"],
      roleCacheTtlSeconds = 60,
      exemptPaths = loginPaths
    }

cfgJson :: Text
cfgJson = "{\"enabled\":true,\"whitelistedRoles\":[\"SUPER_ADMIN\"],\"vpnIps\":[\"10.20.0.0/16\",\"52.1.2.3\"],\"trustedProxies\":[\"10.0.0.0/8\"],\"roleCacheTtlSeconds\":60,\"exemptPaths\":[\"user/login\",\"user/verify2FaSetup\",\"*/*/fleet/v2/login/otp\",\"*/*/ticketdashboard/login/verify\",\"CAC/auth\"]}"

role :: Maybe Text -> IO (Either () (Maybe Text))
role = pure . Right

seg :: Text -> [Text]
seg = filter (not . T.null) . T.splitOn "/"

spec :: Spec
spec = do
  describe "parseConfig" $ do
    it "reads a full row" $ parseConfig cfgJson `shouldBe` Right cfg
    it "defaults missing fields to off, empty and no caching" $
      parseConfig "{}" `shouldBe` Right disabledConfig
    it "rejects invalid json" $ parseConfig "{not json" `shouldSatisfy` isLeft

  describe "isExemptPath" $ do
    it "matches exact and wildcard patterns from the config" $ do
      isExemptPath loginPaths (seg "direct-dashboard/user/login") `shouldBe` True
      isExemptPath loginPaths (seg "direct-dashboard/NAMMA_YATRI_PARTNER/Bangalore/fleet/v2/login/otp") `shouldBe` True
      isExemptPath loginPaths (seg "direct-dashboard/NAMMA_YATRI/Chennai/ticketdashboard/login/verify") `shouldBe` True
      isExemptPath loginPaths (seg "direct-dashboard/CAC/auth") `shouldBe` True
    it "does not match paths missing from the config" $ do
      isExemptPath loginPaths (seg "direct-dashboard/user/logout") `shouldBe` False
      isExemptPath loginPaths (seg "direct-dashboard/internal/auth") `shouldBe` False
      isExemptPath loginPaths (seg "direct-dashboard/user/profile") `shouldBe` False
    it "does not match a longer path or a path outside /direct-dashboard" $ do
      isExemptPath loginPaths (seg "direct-dashboard/user/login/extra") `shouldBe` False
      isExemptPath loginPaths (seg "ui/user/login") `shouldBe` False
    it "exempts nothing with an empty list or empty patterns" $ do
      isExemptPath [] (seg "direct-dashboard/user/login") `shouldBe` False
      isExemptPath ["", "/"] (seg "direct-dashboard") `shouldBe` False

  describe "decide" $ do
    let dd = seg "direct-dashboard/NAMMA_YATRI_PARTNER/Bangalore/driver/list"
        tok = Just "tok"
    it "allows everything when disabled" $
      decide cfg {enabled = False} dd tok (Just "1.1.1.1") (role (Just "OPS")) `shouldReturn` Allow
    it "ignores paths outside /direct-dashboard" $
      decide cfg (seg "ui/driver/profile") tok (Just "1.1.1.1") (role (Just "OPS")) `shouldReturn` Allow
    it "allows configured exempt paths even with a token" $
      decide cfg (seg "direct-dashboard/user/login") tok (Just "1.1.1.1") (role (Just "OPS")) `shouldReturn` Allow
    it "checks login too when the exempt list is empty" $
      decide cfg {exemptPaths = []} (seg "direct-dashboard/user/login") tok (Just "1.1.1.1") (role (Just "OPS")) `shouldReturn` DenyOutsideVpn "OPS" (Just "1.1.1.1")
    it "allows requests without a token" $
      decide cfg dd Nothing (Just "1.1.1.1") (role (Just "OPS")) `shouldReturn` Allow
    it "leaves a token that is not a session to the normal auth" $
      decide cfg dd tok (Just "1.1.1.1") (role Nothing) `shouldReturn` Allow
    it "allows a whitelisted role from anywhere" $
      decide cfg dd tok (Just "1.1.1.1") (role (Just "SUPER_ADMIN")) `shouldReturn` Allow
    it "allows another role from a VPN address" $ do
      decide cfg dd tok (Just "10.20.4.5") (role (Just "OPS")) `shouldReturn` Allow
      decide cfg dd tok (Just "52.1.2.3") (role (Just "OPS")) `shouldReturn` Allow
    it "blocks another role outside the VPN" $ do
      decide cfg dd tok (Just "10.21.0.1") (role (Just "OPS")) `shouldReturn` DenyOutsideVpn "OPS" (Just "10.21.0.1")
      decide cfg dd tok Nothing (role (Just "OPS")) `shouldReturn` DenyOutsideVpn "OPS" Nothing
    it "matches role names exactly" $
      decide cfg dd tok (Just "1.1.1.1") (role (Just "super_admin")) `shouldReturn` DenyOutsideVpn "super_admin" (Just "1.1.1.1")
    it "fails closed when the role lookup fails" $
      decide cfg dd tok (Just "10.20.4.5") (pure (Left ())) `shouldReturn` DenyLookupFailed
    it "does not look the role up when it is not needed" $ do
      let boom = error "role looked up" :: IO (Either () (Maybe Text))
      decide cfg (seg "ui/x") tok Nothing boom `shouldReturn` Allow
      decide cfg dd Nothing Nothing boom `shouldReturn` Allow

  describe "clientIpOf" $ do
    let withXff v = defaultRequest {requestHeaders = [("x-forwarded-for", v)]}
    it "takes the client from X-Forwarded-For, skipping trusted hops" $
      clientIpOf 3 ["10.0.0.0/8"] (withXff "203.0.113.9,10.6.2.152, 136.110.179.139,35.191.69.247") `shouldBe` Just "203.0.113.9"
    it "ignores entries spoofed on the left" $
      clientIpOf 3 [] (withXff "10.20.0.1, 203.0.113.9, 136.110.179.139,35.191.57.76") `shouldBe` Just "203.0.113.9"
  describe "socketHost" $ do
    it "strips the port" $ socketHost "127.0.0.1:51234" `shouldBe` Just "127.0.0.1"
    it "unwraps ipv6" $ socketHost "[::1]:51234" `shouldBe` Just "::1"

  describe "dashboardVpnGuardMiddleware" $ do
    let req ip = defaultRequest {pathInfo = ["direct-dashboard", "M", "C", "driver", "list"], requestHeaders = [("token", "tok"), ("x-forwarded-for", ip)]}
    it "blocks outside the VPN, passes inside it, and logs the block" $ do
      env <- mkEnv (Right (Just cfgJson))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "1.1.1.1") `shouldReturn` status403
      run mw (req "10.20.9.9") `shouldReturn` status200
      readIORef env.logs >>= (`shouldSatisfy` any ("blocked role=OPS ip=1.1.1.1 path=/direct-dashboard/M/C/driver/list" `T.isInfixOf`))
    it "is off when the row does not exist" $ do
      env <- mkEnv (Right Nothing)
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "1.1.1.1") `shouldReturn` status200
    it "reads the config only for dashboard requests with a token" $ do
      env <- mkEnv (Right (Just cfgJson))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      readIORef env.fetches `shouldReturn` 1
      run mw defaultRequest {pathInfo = ["ui", "driver", "profile"], requestHeaders = [("token", "tok")]} `shouldReturn` status200
      run mw defaultRequest {pathInfo = ["direct-dashboard", "user", "login"]} `shouldReturn` status200
      run mw defaultRequest {pathInfo = []} `shouldReturn` status200
      readIORef env.fetches `shouldReturn` 1
      run mw (req "1.1.1.1") `shouldReturn` status403
      readIORef env.fetches `shouldReturn` 2
    it "fails closed until the first config read succeeds" $ do
      env <- mkEnv (Left ())
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "10.20.9.9") `shouldReturn` status503
      run mw defaultRequest {pathInfo = ["direct-dashboard", "user", "login"]} `shouldReturn` status200
      run mw defaultRequest {pathInfo = ["ui", "driver", "profile"], requestHeaders = [("token", "tok")]} `shouldReturn` status200
      writeIORef env.row (Right (Just cfgJson))
      run mw (req "1.1.1.1") `shouldReturn` status403
      run mw (req "10.20.9.9") `shouldReturn` status200
    it "fails closed when the row is invalid json at startup" $ do
      env <- mkEnv (Right (Just "{not json"))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "10.20.9.9") `shouldReturn` status503
    it "picks up a changed row without a restart" $ do
      env <- mkEnv (Right Nothing)
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "1.1.1.1") `shouldReturn` status200
      writeIORef env.row (Right (Just cfgJson))
      run mw (req "1.1.1.1") `shouldReturn` status403
    it "keeps the last good config when the row turns invalid or the read fails" $ do
      env <- mkEnv (Right (Just cfgJson))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      writeIORef env.row (Right (Just "{not json"))
      run mw (req "1.1.1.1") `shouldReturn` status403
      writeIORef env.row (Left ())
      run mw (req "1.1.1.1") `shouldReturn` status403
    it "looks the role up on every request when the cache ttl is 0" $ do
      env <- mkEnv (Right (Just "{\"enabled\":true,\"vpnIps\":[\"10.20.0.0/16\"]}"))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      replicateM_ 3 (run mw (req "1.1.1.1"))
      readIORef env.lookups `shouldReturn` 3
    it "caches the role for the configured ttl" $ do
      env <- mkEnv (Right (Just cfgJson))
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      replicateM_ 3 (run mw (req "1.1.1.1"))
      readIORef env.lookups `shouldReturn` 1
    it "does not cache a token that is not a session" $ do
      env <- mkEnv (Right (Just cfgJson))
      answers <- newIORef [Nothing, Just "OPS"]
      mw <-
        dashboardVpnGuardMiddleware
          (backendOf env)
            { lookupRoleName = \_ -> atomicModifyIORef' answers (\xs -> (drop 1 xs, fromMaybe Nothing (listToMaybe xs)))
            }
      run mw (req "1.1.1.1") `shouldReturn` status200
      run mw (req "1.1.1.1") `shouldReturn` status403
    it "fails closed when the role lookup fails" $ do
      env <- mkEnv (Right (Just cfgJson))
      writeIORef env.lookupFails True
      mw <- dashboardVpnGuardMiddleware (backendOf env)
      run mw (req "10.20.9.9") `shouldReturn` status503
    it "lets a timeout or cancellation through instead of answering 503" $ do
      env <- mkEnv (Right (Just cfgJson))
      mw <- dashboardVpnGuardMiddleware (backendOf env) {lookupRoleName = \_ -> E.throwIO E.ThreadKilled}
      run mw (req "1.1.1.1") `shouldThrow` (== E.ThreadKilled)

data Env = Env
  { row :: IORef (Either () (Maybe Text)),
    logs :: IORef [Text],
    lookups :: IORef Int,
    lookupFails :: IORef Bool,
    fetches :: IORef Int
  }

mkEnv :: Either () (Maybe Text) -> IO Env
mkEnv initial = Env <$> newIORef initial <*> newIORef [] <*> newIORef 0 <*> newIORef False <*> newIORef 0

backendOf :: Env -> DashboardVpnGuardBackend
backendOf env =
  DashboardVpnGuardBackend
    { fetchConfig = \_ -> modifyIORef' env.fetches (+ 1) >> readIORef env.row >>= either (\() -> throwIO (userError "db down")) pure,
      lookupRoleName = \t -> do
        modifyIORef' env.lookups (+ 1)
        failing <- readIORef env.lookupFails
        when failing $ throwIO (userError "db down")
        pure (if t == "tok" then Just "OPS" else Nothing),
      logInfoIO = \l -> modifyIORef' env.logs (l :),
      logErrorIO = \l -> modifyIORef' env.logs (l :),
      clientIpPositionFromRight = 1,
      refreshSeconds = 0
    }

ok :: Application
ok _ respond = respond (responseLBS status200 [] "ok")

run :: (Application -> Application) -> Request -> IO Status
run mw r = do
  ref <- newIORef status200
  _ <- mw ok r (\res -> modifyIORef' ref (const (responseStatus res)) >> pure ResponseReceived)
  readIORef ref
