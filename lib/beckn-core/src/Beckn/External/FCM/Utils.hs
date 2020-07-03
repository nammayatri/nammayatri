module Beckn.External.FCM.Utils where

import qualified Beckn.External.FCM.Flow as FCM
import qualified Beckn.Utils.JWT as JWT
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified System.Environment as SE

-- | Create a loop that refreshes FCM token in background
createFCMTokenRefreshThread :: L.Flow ()
createFCMTokenRefreshThread = do
  fcmEnabled <- L.runIO $ SE.lookupEnv "FCM_JSON_PATH"
  case fcmEnabled of
    Nothing -> pure () --report error here if FCM is crucial
    Just _ -> L.forkFlow forkDesc $ do
      _ <- forever $ do
        token <- FCM.checkAndGetToken
        L.runIO $ delay token
      pure ()
  where
    forkDesc = "Forever loop that checks and refreshes FCM token"
    -- wait for some time depeding on a token status
    delay token =
      case token of
        Left _ -> threadDelay $ 5 * 1000000 -- bad token? retry
        Right t -> do
          validityStatus <- JWT.isValid t
          threadDelay $
            1000000 * case validityStatus of
              JWT.JWTValid x ->
                -- seconds before token expiration
                if x > 300
                  then fromInteger x - 300
                  else 10
              _ -> 10 -- just a caution, it should be valid by this moment
