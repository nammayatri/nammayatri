{-# LANGUAGE OverloadedStrings #-}

-- | Fetches the Juspay portal transaction-export CSV directly via
-- @<portalBaseUrl>/api/q/download@.
--
-- Unlike SFTP/Email sources this doesn't rely on the PG pushing a settlement
-- file — it pulls the portal's own transaction dashboard export, scoped to
-- one IST day at a time. Auth is an OAuth Bearer token stored encrypted in
-- 'JuspayApiConfig'; the token has a ~90 day expiry and must be rotated
-- manually. A 401/403 (or an HTML response, which the portal returns on
-- auth failure) surfaces as a @Left@ so the job logs and re-schedules
-- rather than silently ingesting garbage.
--
-- Dedup mirrors the SFTP model: a synthetic per-day filename
-- @juspay_portal_YYYY-MM-DD.csv@ is tracked in @settlement_file_info@.
-- A COMPLETED row for today's window short-circuits the fetch to an empty
-- CSV (0 rows ingested, no meta); a PENDING row is reused so partial
-- failures re-run against the same tracker id.
module Lib.Finance.Settlement.Sources.JuspayApi
  ( fetchSettlementFile,
    yesterdayIstUtcBounds,
    buildQueryPayload,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    TimeZone (..),
    addDays,
    defaultTimeLocale,
    formatTime,
    localTimeToUTC,
    utcToLocalTime,
  )
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (JuspayApiConfig (..))
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime, logInfo)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.Sources.SFTP (SftpFetchMeta (..))
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import qualified Prelude as P

-- | Fetch yesterday's IST day of portal transactions. Idempotent per day via
-- @settlement_file_info@: returns @(bs, Just meta)@ when new data was pulled
-- (ingestion will flip PENDING → COMPLETED on the returned trackedFileId);
-- returns @(\"\", Nothing)@ when today's window is already COMPLETED, so the
-- ingestion pipeline safely no-ops.
fetchSettlementFile ::
  (BeamFlow m r, EncFlow m r, MonadIO m) =>
  Text ->
  Text ->
  Text ->
  JuspayApiConfig ->
  m (Either Text (LBS.ByteString, Maybe SftpFetchMeta))
fetchSettlementFile merchantId merchantOperatingCityId paymentGatewayName config = do
  now <- getCurrentTime
  let (startUtc, endUtc) = yesterdayIstUtcBounds now
      dayLabel = formatTime defaultTimeLocale "%Y-%m-%d" startUtc
      trackerFileName = T.pack $ "juspay_portal_" <> dayLabel <> ".csv"
  mbExisting <-
    QSFI.findByMerchantCityGatewayAndFileName
      merchantId
      merchantOperatingCityId
      paymentGatewayName
      trackerFileName
  case mbExisting of
    Just row | row.status == COMPLETED -> do
      logInfo $
        "JuspayApi.fetchSettlementFile: window already COMPLETED, skipping. "
          <> "fileName="
          <> trackerFileName
          <> " trackedFileId="
          <> getId row.id
      pure $ Right ("", Nothing)
    Just row -> do
      logInfo $
        "JuspayApi.fetchSettlementFile: reusing PENDING tracker "
          <> "fileName="
          <> trackerFileName
          <> " trackedFileId="
          <> getId row.id
          <> " previousStatus="
          <> T.pack (show row.status)
      pullAndReturn config startUtc endUtc row.id
    Nothing -> do
      newIdRaw <- generateGUID
      let sfId = Id newIdRaw
          row =
            SettlementFileInfo
              { id = sfId,
                paymentGatewayName,
                fileName = trackerFileName,
                status = PENDING,
                lastProcessedIndex = -1,
                merchantId,
                merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      QSFI.create row
      logInfo $
        "JuspayApi.fetchSettlementFile: created new tracker "
          <> "fileName="
          <> trackerFileName
          <> " trackedFileId="
          <> getId sfId
      pullAndReturn config startUtc endUtc sfId

pullAndReturn ::
  (EncFlow m r, MonadIO m) =>
  JuspayApiConfig ->
  UTCTime ->
  UTCTime ->
  Id SettlementFileInfo ->
  m (Either Text (LBS.ByteString, Maybe SftpFetchMeta))
pullAndReturn config startUtc endUtc trackedFileId = do
  eBytes <- pullPortalCsv config startUtc endUtc
  case eBytes of
    Left err -> pure $ Left err
    Right bs -> do
      let meta =
            SftpFetchMeta
              { trackedFileId = trackedFileId,
                firstDataRowIndex = 0,
                dataRowsDelivered = countCsvDataRows bs,
                atomicPull = True
              }
      pure $ Right (bs, Just meta)

pullPortalCsv ::
  (EncFlow m r, MonadIO m) =>
  JuspayApiConfig ->
  UTCTime ->
  UTCTime ->
  m (Either Text LBS.ByteString)
pullPortalCsv config startUtc endUtc = do
  token <- decrypt config.oauthToken
  let queryJson = buildQueryPayload config.juspayMerchantId startUtc endUtc
      filenameHint =
        "juspay_portal_" <> formatTime defaultTimeLocale "%Y-%m-%d" startUtc <> ".csv"
      baseUrlStr = T.unpack (showBaseUrl config.portalBaseUrl)
      url = baseUrlStr <> "/api/q/download"
      ua =
        maybe
          "Mozilla/5.0 (Linux; Android 15; Pixel 9) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/149.0.0.0 Mobile Safari/537.36"
          T.unpack
          config.userAgent
  (exitCode, stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "curl"
        [ "--silent",
          "--show-error",
          "--fail-with-body",
          "-G",
          "--max-time",
          "300",
          "--data-urlencode",
          "query=" <> LBSC.unpack (A.encode queryJson),
          "--data-urlencode",
          "filename=" <> filenameHint,
          "--data-urlencode",
          "format=csv",
          "-H",
          "Authorization: Bearer " <> T.unpack token,
          "-H",
          "Accept: */*",
          "-H",
          "Referer: " <> baseUrlStr <> "/",
          "-H",
          "User-Agent: " <> ua,
          url
        ]
        ""
  case exitCode of
    ExitFailure code ->
      pure $
        Left $
          "Juspay portal /api/q/download failed (curl exit " <> T.pack (P.show code) <> "): "
            <> T.pack stderr
    ExitSuccess -> do
      let bodyBs = LBSC.pack stdout
      if looksLikeHtml bodyBs
        then
          pure $
            Left
              "Juspay portal returned HTML instead of CSV — likely expired OAuth token (JuspayApiConfig.oauthToken)"
        else pure $ Right bodyBs

-- | Portal auth failures return an HTML login page with 200 OK. Detect by
-- leading @<@ after skipping BOM/whitespace.
looksLikeHtml :: LBS.ByteString -> Bool
looksLikeHtml bs =
  case LBSC.uncons (LBSC.dropWhile (\c -> Char.isSpace c || c == '\xEF' || c == '\xBB' || c == '\xBF') bs) of
    Just ('<', _) -> True
    _ -> False

-- | Count non-empty data rows (header excluded). Used only for observability
-- via 'SftpFetchMeta.dataRowsDelivered'.
countCsvDataRows :: LBS.ByteString -> Int
countCsvDataRows bs =
  case LBSC.split '\n' bs of
    (_hdr : rest) -> length (filter (not . LBS.null . LBSC.filter (/= '\r')) rest)
    [] -> 0

-- | @[yesterday-00:00-IST, today-00:00-IST)@ expressed in UTC. Called at job
-- run-time so the interval slides forward each day.
yesterdayIstUtcBounds :: UTCTime -> (UTCTime, UTCTime)
yesterdayIstUtcBounds now =
  let ist = TimeZone 330 False "IST"
      todayIst :: Day
      todayIst = localDay' (utcToLocalTime ist now)
      yesterdayIst = addDays (-1) todayIst
      startUtc = localTimeToUTC ist (LocalTime yesterdayIst (TimeOfDay 0 0 0))
      endUtc = localTimeToUTC ist (LocalTime todayIst (TimeOfDay 0 0 0))
   in (startUtc, endUtc)
  where
    localDay' (LocalTime d _) = d

-- | Construct the portal /api/q/download JSON query for a single day window.
-- Column set covers what the portal parser currently reads; extend the
-- @metrics@ list here if the parser starts consuming additional columns.
buildQueryPayload :: Text -> UTCTime -> UTCTime -> A.Value
buildQueryPayload juspayMerchantId startUtc endUtc =
  A.object
    [ "metrics" A..= metrics,
      "domain" A..= ("txn" :: Text),
      "interval"
        A..= A.object
          [ "start" A..= isoUtc startUtc,
            "end" A..= isoUtc endUtc
          ],
      "midFilter"
        A..= A.object
          [ "field" A..= ("merchant_id" :: Text),
            "condition" A..= ("In" :: Text),
            "val" A..= [juspayMerchantId]
          ],
      "with"
        A..= [ istColumn "txn_last_modified",
               istColumn "execution_date",
               istColumn "order_date_created",
               istColumn "system_date",
               istColumn "refund_date"
             ]
    ]
  where
    isoUtc = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
    metricNames :: [Text]
    metricNames =
      [ "order_id",
        "txn_uuid",
        "juspay_txn_id",
        "epg_txn_id",
        "rrn",
        "amount",
        "effective_txn_amount",
        "settlement_amount",
        "tax_amount",
        "discount_amount",
        "offer_total_discount_amount",
        "order_amount_refunded",
        "payment_status",
        "order_status",
        "payment_method_type",
        "payment_method_subtype",
        "payment_gateway",
        "bank",
        "order_currency",
        "order_date_created",
        "system_date",
        "execution_date",
        "refund_date",
        "settlement_epg_id",
        "sub_vendor_id",
        "gateway_reference_id",
        "card_last_four_digits",
        "card_brand",
        "card_sub_type",
        "card_bin",
        "arn",
        "auth_code",
        "merchant_offer_code",
        "offer_ids"
      ]
    metrics =
      [ A.object ["key" A..= k, "as" A..= k]
        | k <- metricNames
      ]
    istColumn c =
      A.object
        [ "column" A..= (c :: Text),
          "time_zone" A..= ("Asia/Kolkata" :: Text),
          "name" A..= (c :: Text)
        ]
