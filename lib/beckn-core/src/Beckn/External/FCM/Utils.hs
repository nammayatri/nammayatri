{-# LANGUAGE TypeApplications #-}

module Beckn.External.FCM.Utils where

import qualified Beckn.External.FCM.Flow as FCM
import Beckn.Types.Common
import qualified Beckn.Utils.JWT as JWT
import qualified Control.Exception as E (try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified EulerHS.Language as L
import EulerHS.Prelude
import GHC.Records (HasField (..))

-- | Create a loop that refreshes FCM token in background
createFCMTokenRefreshThread :: HasField "fcmJsonPath" r (Maybe Text) => FlowR r ()
createFCMTokenRefreshThread =
  getField @"fcmJsonPath" <$> ask
    >>= maybe (pure ()) withFJP -- report error here if FCM is crucial
  where
    withFJP f = L.runIO (readAndDecode f) >>= either logIt doIt
    readAndDecode f = either (Left . excText f) (first fromString . Aeson.eitherDecode) <$> E.try (BL.readFile $ toString f)
    excText f e = "Error on reading FCM json file [" <> f <> "]: " <> fromString (displayException (e :: SomeException))
    doIt sa = lift . L.forkFlow forkDesc $ do
      _ <- forever $ do
        token <- FCM.checkAndGetToken sa
        L.runIO $ delay token
      pure ()
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
    logIt = L.logInfo ("fcm" :: Text)
