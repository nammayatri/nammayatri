{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Common
  ( module Beckn.Utils.Common,
    module Beckn.Utils.Logging,
    module Beckn.Utils.Error,
    module Beckn.Types.Flow,
    module Beckn.Utils.Servant.Client,
    module Beckn.Utils.Context,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack as Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Types.Error
import Beckn.Types.Error.APIError
import Beckn.Types.Field
import Beckn.Types.Flow
import Beckn.Utils.Context
import Beckn.Utils.Error
import Beckn.Utils.Logging
import Beckn.Utils.Servant.Client
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import Data.Time.Units (TimeUnit, fromMicroseconds)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import GHC.Records (HasField (..))
import qualified Servant.Client as S

roundDiffTimeToUnit :: TimeUnit u => NominalDiffTime -> u
roundDiffTimeToUnit = fromMicroseconds . round . (* 1e6)

mkOkResponse :: MonadTime m => Context -> m AckResponse
mkOkResponse context = do
  currTime <- getCurrentTime
  let context' = context {_timestamp = currTime}
  return $ AckResponse context' (ack Ack.ACK) Nothing

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

authenticate ::
  ( HasField "cronAuthKey" r (Maybe CronAuthKey)
  ) =>
  Maybe CronAuthKey ->
  FlowR r ()
authenticate = check handleKey
  where
    handleKey rauth = do
      key <- check pure =<< getField @"cronAuthKey" <$> ask
      check (flip when throw401 . (key /=)) $
        DT.decodeUtf8 <$> (rightToMaybe . DBB.decode . DT.encodeUtf8 =<< T.stripPrefix "Basic " rauth)
    check = maybe throw401
    throw401 :: FlowR r a
    throw401 = throwError (AuthBlocked "Bad auth key")

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst time =
  T.pack $
    Time.formatTime Time.defaultTimeLocale "%d %b, %I:%M %p" $
      Time.addUTCTime (60 * 330) time

-- | A replacement for 'L.forkFlow' which works in 'FlowR'.
-- It's main use case is to perform an action asynchronously without waiting for
-- result.
--
-- It has several differences comparing to 'L.forkFlow':
-- * Logs errors in case if the action failed;
-- * Expects action to return '()' - this is good, because the opposite means
--   you ignored something important, e.g. an exception returned explicitly;
-- * Do not log the fact of thread creation (was it any useful?)
--
-- NOTE: this function is temporary, use of bare forking is bad and should be
-- removed one day.

-- I know it is looking similar to forkAsync but I found it simpler to
-- be able to use (FlowR r) instead of (FlowR (EnvR r))
fork :: Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ handleExc env $ runReaderT f env
  where
    handleExc env a =
      L.runSafeFlow a >>= \case
        Right () -> pass
        Left e -> runReaderT (err e) env
    err e =
      logTagWarning "Thread" $
        "Thread " <> show desc <> " died with error: " <> show e

runSafeFlow :: (FromJSON a, ToJSON a) => FlowR r a -> FlowR r (Either Text a)
runSafeFlow flow = do
  env <- ask
  lift $ L.runSafeFlow $ runReaderT flow env

addIfPresent :: [a] -> Maybe a -> [a]
addIfPresent xs (Just x) = x : xs
addIfPresent xs _ = xs

isExpired :: MonadTime m => NominalDiffTime -> UTCTime -> m Bool
isExpired nominal time = do
  now <- getCurrentTime
  let addedUTCTime = Time.addUTCTime nominal time
  return $ now > addedUTCTime

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

padLeft :: Int -> Char -> Text -> Text
padLeft n c txt =
  let prefix = replicate (max 0 $ n - length txt) c
   in T.pack prefix <> txt

-- Suits only for non-negative numbers
padNumber :: Integral i => Int -> i -> Text
padNumber n num = padLeft n '0' $ show (fromIntegral num :: Natural)

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( L.MonadFlow m,
    MonadReader r m,
    HasFields r fields
  )

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

checkAckResponseError :: (MonadThrow m, Log m, IsAPIException e) => (Error -> e) -> AckResponse -> m ()
checkAckResponseError err ackResp = whenJust (ackResp ^. #_error) (throwError . err)

parseBaseUrl :: L.MonadFlow m => Text -> m S.BaseUrl
parseBaseUrl url =
  L.runIO $
    S.parseBaseUrl $ T.unpack url
