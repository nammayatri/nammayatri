-- | RADAR ticket desk: the Control Center support view over Xyne Spaces.
--
-- Configuration rides the platform's standard per-merchant service-config
-- machinery: each merchant-operating-city has its own
-- @merchant_service_config@ row with @service_name = Ticket_RadarXyneSpaces@,
-- whose payload is the same 'XyneCfg.XyneSpacesCfg' shape the in-app issue
-- desk uses (separate row, separate Xyne app\/JWT\/channel — full isolation
-- from the in-app desk). The row's ABSENCE is the per-merchant feature gate.
--
-- Call style mirrors the in-app desk: reads and CSAT go through
-- 'Kernel.External.Ticket.Interface.XyneSpaces' (token decrypted inside via
-- passetto, typed 'XyneError's); reply\/create\/updateStatus decrypt the
-- token and call the Flow clients directly, because the Interface's
-- 'createTicket'\/'updateTicket' compose issue-desk-specific subjects and
-- bodies and carry no @senderEmail@\/@externalId@\/@isNew@ handling, all of
-- which RADAR's thread-exact, operator-attributed writes require.
--
-- Read behaviour is a 1:1 port of the (live-verified) control-center Express
-- implementation: page caps, 500-ticket summary sweep, subject-category
-- parsing, Redis reporter cache, S3-rehosted attachments served as presigned
-- URLs, reply thread-key recovery with the isNew forked-thread guard.
module Domain.Action.Dashboard.Management.RadarTickets
  ( getRadarTicketsList,
    getRadarTicketsSummary,
    getRadarTicketsReporters,
    getRadarTicketsTicket,
    getRadarTicketsTicketConversation,
    getRadarTicketsTicketAttachment,
    postRadarTicketsTicketReply,
    postRadarTicketsUpload,
    postRadarTicketsCreate,
    postRadarTicketsTicketUpdateStatus,
    postRadarTicketsTicketCsat,
  )
where

import qualified API.Types.ProviderPlatform.Management.RadarTickets as Common
import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Utils as DU
import qualified Environment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.Interface.XyneSpaces as XyneIf
import qualified Kernel.External.Ticket.XyneSpaces.Config as XyneCfg
import qualified Kernel.External.Ticket.XyneSpaces.Flow as XF
import qualified Kernel.External.Ticket.XyneSpaces.Types as XyneTypes
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import System.Directory (getTemporaryDirectory, removeFile)

-- Behavioural constants carried over verbatim from the control-center
-- implementation these endpoints replace.
maxPageSize :: Int
maxPageSize = 100

sweepPageSize :: Int
sweepPageSize = 100

summaryMaxTickets :: Int
summaryMaxTickets = 500

maxReplyChars :: Int
maxReplyChars = 5000

reporterBatchSize :: Int
reporterBatchSize = 10

writeRateLimitOptions :: APIRateLimitOptions
writeRateLimitOptions = APIRateLimitOptions {limit = 10, limitResetTimeInSec = 600}

-- | A ticket's reporter never changes; the Express version cached for the
-- process lifetime, here 30 days of Redis stands in for that.
reporterCacheTtl :: Int
reporterCacheTtl = 30 * 24 * 3600

attachmentCacheTtl :: Int
attachmentCacheTtl = 30 * 24 * 3600

attachmentUrlExpiry :: Seconds
attachmentUrlExpiry = 3600

-- | Staged uploads are transient: the frontend uploads screenshots, then
-- immediately references the returned fileIds in the create call. An hour is
-- generous; expired ids fail the create with a clear message.
uploadStagingTtl :: Int
uploadStagingTtl = 3600

maxUploadBytes :: Int
maxUploadBytes = 10 * 1024 * 1024

-- Cache keys are scoped by merchant-operating-city id. A ticket/attachment id
-- from one merchant's Xyne channel must never resolve a cache entry populated
-- by another merchant — the per-merchant Xyne token is the only authz boundary
-- on the upstream fetch, and an unscoped cache hit would bypass it.
reporterCacheKey :: Text -> Text -> Text
reporterCacheKey mocId ticketId = "radar:xyne:reporter:" <> mocId <> ":" <> ticketId

attachmentCacheKey :: Text -> Text -> Text
attachmentCacheKey mocId attachmentId = "radar:xyne:attachment:s3:" <> mocId <> ":" <> attachmentId

uploadStagingKey :: Text -> Text
uploadStagingKey fileId = "radar:upload:" <> fileId

-- | A screenshot/file staged by @/upload@, waiting to be attached by a
-- subsequent @/create@. Held in Redis (base64) — small files, short TTL.
data StagedUpload = StagedUpload
  { fileName :: Text,
    mimeType :: Text,
    contentBase64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

-- | Resolve this merchant-city's RADAR desk config: the
-- @Ticket_RadarXyneSpaces@ merchant_service_config row. Mirrors the in-app
-- desk's lookupXyneCfg (Domain.Action.UI.XyneWebhook), but for the RADAR
-- row and driver-app id types. The row's absence = RADAR not enabled here.
lookupRadarXyneCfg :: Text -> Environment.Flow XyneCfg.XyneSpacesCfg
lookupRadarXyneCfg mocId = do
  mbMsc <-
    getOneConfig
      (MerchantServiceConfigDimensions {merchantOperatingCityId = mocId, merchantId = Nothing, serviceName = Just (DMSC.IssueTicketService Ticket.RadarXyneSpaces)})
      Nothing
  msc <- mbMsc & fromMaybeM (InvalidRequest "RADAR is not configured for this merchant.")
  case msc.serviceConfig of
    DMSC.IssueTicketServiceConfig (Ticket.RadarXyneSpacesConfig cfg) -> pure cfg
    _ -> throwError (InternalError "Unexpected service config shape for RadarXyneSpaces")

-- | Standard Management-handler preamble: merchant + operating city + the
-- RADAR row. Every endpoint of this module starts here.
resolveRadarCfg :: ShortId DM.Merchant -> Context.City -> Environment.Flow XyneCfg.XyneSpacesCfg
resolveRadarCfg merchantShortId opCity = snd <$> resolveRadarCfgWithMoc merchantShortId opCity

-- | Same as 'resolveRadarCfg' but also returns the merchant-operating-city id,
-- for handlers that scope their Redis\/S3 cache keys by merchant.
resolveRadarCfgWithMoc :: ShortId DM.Merchant -> Context.City -> Environment.Flow (Text, XyneCfg.XyneSpacesCfg)
resolveRadarCfgWithMoc merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  cfg <- lookupRadarXyneCfg merchantOpCityId.getId
  pure (merchantOpCityId.getId, cfg)

-- | 'XyneNotFound' → a caller-facing not-found; every other 'XyneError'
-- (unauthorized, 5xx, …) propagates with its own typed code.
translateNotFound :: Text -> Environment.Flow a -> Environment.Flow a
translateNotFound msg action = do
  res <- try @_ @XyneError action
  case res of
    Left XyneNotFound -> throwError (InvalidRequest msg)
    Left err -> throwM err
    Right a -> pure a

getRadarTicketsList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Environment.Flow Common.RadarTicketListRes
getRadarTicketsList merchantShortId opCity mbPageSize mbCursor mbPriority mbStageName mbReporterEmail = do
  cfg <- resolveRadarCfg merchantShortId opCity
  mbPriorityEnum <- mapM parsePriority (nonBlank =<< mbPriority)
  let pageSize = min maxPageSize (max 1 (fromMaybe 25 mbPageSize))
      mbFilters = mkFilters mbPriorityEnum (nonBlank =<< mbStageName)
  resp <- XyneIf.listTickets cfg (mkListReq cfg pageSize (nonBlank =<< mbCursor) mbFilters (nonBlank =<< mbReporterEmail))
  nowIso <- nowIsoText
  pure
    Common.RadarTicketListRes
      { items = map (mkTicketItem nowIso) resp.items,
        hasMore = fromMaybe False resp.hasMore,
        nextCursor = resp.nextCursor
      }

getRadarTicketsSummary :: ShortId DM.Merchant -> Context.City -> Environment.Flow Common.RadarSummaryRes
getRadarTicketsSummary merchantShortId opCity = do
  cfg <- resolveRadarCfg merchantShortId opCity
  rawItems <- sweepTickets cfg
  nowIso <- nowIsoText
  let allTickets = map (mkTicketItem nowIso) rawItems
      inStage stage = length (filter (\t -> maybe "" T.toUpper t.stageName == stage) allTickets)
      tiles =
        Common.RadarSummaryTiles
          { total = length allTickets,
            backlog = inStage "BACKLOG",
            inProgress = inStage "IN PROGRESS",
            completed = inStage "COMPLETED",
            notRequired = inStage "NOT REQUIRED"
          }
      counts = Map.toList (Map.fromListWith (+) [(categoryFromSubject t.subject, 1 :: Int) | t <- allTickets])
      byCategory = map (\(cat, cnt) -> Common.RadarCategoryCount {category = cat, count = cnt}) (List.sortOn (negate . snd) counts)
  pure Common.RadarSummaryRes {tiles = tiles, recent = take 8 allTickets, byCategory = byCategory}

getRadarTicketsReporters :: ShortId DM.Merchant -> Context.City -> Environment.Flow Common.RadarReportersRes
getRadarTicketsReporters merchantShortId opCity = do
  (mocId, cfg) <- resolveRadarCfgWithMoc merchantShortId opCity
  rawItems <- sweepTickets cfg
  let ticketIds = List.nub (mapMaybe (\item -> nonBlank =<< item.ticketId) rawItems)
  -- The list response carries no sender, so each uncached reporter costs a
  -- detail fetch. Batched concurrency keeps a cold cache from serialising
  -- hundreds of upstream calls; per-ticket failures are swallowed so one
  -- unreadable ticket cannot empty the dropdown.
  emails <- fmap (catMaybes . concat) $
    forM (chunksOf reporterBatchSize ticketIds) $ \batch ->
      DU.mapConcurrently (resolveReporterEmail mocId cfg) batch
  let reporters = List.sortOn (.name) [Common.RadarReporter {email = e, name = nameFromEmail e} | e <- List.nub emails]
  pure Common.RadarReportersRes {reporters = reporters}

getRadarTicketsTicket :: ShortId DM.Merchant -> Context.City -> Text -> Environment.Flow Common.RadarTicketDetailRes
getRadarTicketsTicket merchantShortId opCity ticketId = do
  cfg <- resolveRadarCfg merchantShortId opCity
  detail <- translateNotFound "Ticket not found." $ XyneIf.getTicket cfg ticketId
  -- Conversation failure degrades to an empty thread rather than failing the
  -- whole detail view (matches the Express behaviour).
  convRes <- try @_ @SomeException (XyneIf.getTicketConversation cfg ticketId)
  let conv = case convRes of
        Right c -> c
        Left _ -> XyneTypes.XyneConversationResp {items = [], hasMore = Nothing}
  nowIso <- nowIsoText
  let base = mkTicketItem nowIso (detailAsSummary detail)
  pure
    Common.RadarTicketDetailRes
      { id = base.id,
        xyneId = base.xyneId,
        subject = base.subject,
        priority = base.priority,
        statusV2 = base.statusV2,
        stageName = base.stageName,
        createdAt = base.createdAt,
        lastEmailAt = base.lastEmailAt,
        description = fromMaybe "" (nonBlank =<< detail.description),
        updatedAt = iso8601 <$> detail.updatedAt,
        reporterEmail = reporterFromDetail detail,
        emailCount = detail.emailCount,
        history = maybe [] (zipWith mkEvent [0 ..]) detail.history,
        messages = map mkMessage conv.items
      }

getRadarTicketsTicketConversation :: ShortId DM.Merchant -> Context.City -> Text -> Environment.Flow Common.RadarConversationRes
getRadarTicketsTicketConversation merchantShortId opCity ticketId = do
  cfg <- resolveRadarCfg merchantShortId opCity
  conv <- translateNotFound "Ticket not found." $ XyneIf.getTicketConversation cfg ticketId
  pure Common.RadarConversationRes {messages = map mkMessage conv.items, hasMore = fromMaybe False conv.hasMore}

getRadarTicketsTicketAttachment :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Environment.Flow Common.RadarAttachmentUrlRes
getRadarTicketsTicketAttachment merchantShortId opCity _ticketId attachmentId = do
  (mocId, cfg) <- resolveRadarCfgWithMoc merchantShortId opCity
  (mbCachedPath :: Maybe Text) <- Redis.safeGet (attachmentCacheKey mocId attachmentId)
  filePath <- case mbCachedPath of
    Just p -> pure p
    Nothing -> do
      (bytes, mbContentType) <- translateNotFound "Attachment not found." $ XyneIf.downloadAttachment cfg attachmentId
      let (fileType, ext) = mimeToFileTypeExt mbContentType
          contentType = fromMaybe "application/octet-stream" (nonBlank =<< mbContentType)
      -- moc-scoped S3 key too, so a rehosted object is never shared across merchants.
      newPath <- S3.createFilePath ("radar-attachments/" <> mocId <> "/") ("xyne-" <> attachmentId) fileType ext
      S3.putRaw (T.unpack newPath) (LBS.toStrict bytes) (T.unpack contentType)
      Redis.setExp (attachmentCacheKey mocId attachmentId) newPath attachmentCacheTtl
      pure newPath
  url <- S3.generateDownloadUrl (T.unpack filePath) attachmentUrlExpiry
  pure Common.RadarAttachmentUrlRes {url = url}

postRadarTicketsTicketReply :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarReplyReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketReply merchantShortId opCity ticketId req = do
  cfg <- resolveRadarCfg merchantShortId opCity
  senderEmail <- requireSenderEmail req.senderEmail
  let body' = T.strip req.body
  when (T.null body') $ throwError $ InvalidRequest "Write a message before sending."
  when (T.length body' > maxReplyChars) $ throwError $ InvalidRequest ("Keep the message under " <> show maxReplyChars <> " characters.")
  checkSlidingWindowLimitWithOptions ("radar:reply:rateLimit:" <> senderEmail) writeRateLimitOptions
  detail <- translateNotFound "Ticket not found." $ XyneIf.getTicket cfg ticketId
  conv <- translateNotFound "Ticket not found." $ XyneIf.getTicketConversation cfg ticketId
  -- The reply thread key is the externalThreadId of the FIRST message; only
  -- tickets filed from Control Center carry one.
  firstMsg <- fromMaybeM notFiledFromCC (listToMaybe conv.items)
  threadId <- fromMaybeM notFiledFromCC (nonBlank =<< firstMsg.externalThreadId)
  token <- decrypt cfg.token
  let subject = fromMaybe (fromMaybe "" (nonBlank =<< detail.title)) (nonBlank =<< firstMsg.subject)
      inboundReq =
        XyneTypes.XyneInboundReq
          { channelId = cfg.channelId,
            threadId = threadId,
            subject = subject,
            -- Operator's text forwarded verbatim (plain text, like the in-app
            -- desk's bodies) — we never compose markup, so nothing to escape.
            body = body',
            externalId = Nothing,
            senderName = (nonBlank =<< req.senderName) <|> Just senderEmail,
            senderEmail = Just senderEmail,
            additionalFormFields = Nothing
          }
  resp <- XF.appDeskInboundAPI cfg.url token inboundReq
  -- isNew means the desk opened a brand-new ticket instead of appending to
  -- this one — surface it loudly rather than silently forking the thread.
  when resp.isNew $
    throwError $ InternalError $ "Xyne opened a new ticket (" <> resp.xyneId <> ") instead of replying on " <> ticketId
  pure APISuccess.Success
  where
    notFiledFromCC = InvalidRequest "This ticket wasn't filed from Control Center, so replies must happen on the Xyne desk."

postRadarTicketsUpload :: ShortId DM.Merchant -> Context.City -> Common.RadarUploadReq -> Environment.Flow Common.RadarUploadRes
postRadarTicketsUpload merchantShortId opCity req = do
  void $ resolveRadarCfg merchantShortId opCity
  fileName <- fromMaybeM (InvalidRequest "fileName is required.") (nonBlank req.fileName)
  mimeType <- fromMaybeM (InvalidRequest "mimeType is required.") (nonBlank req.mimeType)
  bytes <- case B64.decode (TE.encodeUtf8 req.fileBase64) of
    Left err -> throwError $ InvalidRequest ("fileBase64 is not valid base64: " <> T.pack err)
    Right b -> pure b
  when (BS.null bytes) $ throwError $ InvalidRequest "Empty file."
  when (BS.length bytes > maxUploadBytes) $ throwError $ InvalidRequest "File exceeds the 10 MB limit."
  fileId <- generateGUID
  Redis.setExp (uploadStagingKey fileId) (StagedUpload {fileName = fileName, mimeType = mimeType, contentBase64 = req.fileBase64}) uploadStagingTtl
  pure Common.RadarUploadRes {fileId = fileId}

postRadarTicketsCreate :: ShortId DM.Merchant -> Context.City -> Common.RadarCreateTicketReq -> Environment.Flow Common.RadarCreateTicketRes
postRadarTicketsCreate merchantShortId opCity req = do
  cfg <- resolveRadarCfg merchantShortId opCity
  senderEmail <- requireSenderEmail req.senderEmail
  title <- fromMaybeM (InvalidRequest "Give the issue a title.") (nonBlank req.title)
  category <- fromMaybeM (InvalidRequest "category is required.") (nonBlank req.category)
  subCategory <- fromMaybeM (InvalidRequest "subCategory is required.") (nonBlank req.subCategory)
  urgencyTag <- urgencyToTag req.urgency
  let body' = T.strip req.body
  when (T.null body') $ throwError $ InvalidRequest "Write a description before sending."
  when (T.length body' > maxReplyChars) $ throwError $ InvalidRequest ("Keep the description under " <> show maxReplyChars <> " characters.")
  checkSlidingWindowLimitWithOptions ("radar:create:rateLimit:" <> senderEmail) writeRateLimitOptions
  staged <- forM req.attachmentFileIds $ \fileId -> do
    (mbStaged :: Maybe StagedUpload) <- Redis.safeGet (uploadStagingKey fileId)
    mbStaged & fromMaybeM (InvalidRequest $ "Uploaded file " <> fileId <> " has expired or does not exist; upload it again.")
  uuid <- generateGUID
  token <- decrypt cfg.token
  let threadId = "radar-" <> uuid
      senderName = fromMaybe senderEmail (nonBlank =<< req.senderName)
      cities = T.intercalate ", " (filter (not . T.null . T.strip) req.citiesAffected)
      subject = urgencyTag <> " [Control Center] " <> category <> " › " <> subCategory <> ": " <> title <> (if T.null cities then "" else " — " <> cities)
      formFields =
        [ ("Category", category <> " › " <> subCategory),
          ("Cities", cities),
          ("Where seen", req.whereSeen),
          ("Still happening", req.stillHappening),
          ("Priority", T.toUpper req.urgency)
        ]
      contextFields = [(kv.key, kv.value) | kv <- req.context, isJust (nonBlank kv.value)]
      -- Minimal payload: the six fields the API needs. No externalId (it's a
      -- dedup key and every call here has a fresh guid — nothing to dedup),
      -- no additionalFormFields (this board has no form; Xyne rejects them),
      -- and the reporter travels as senderName/senderEmail, which the desk
      -- surfaces as the message's from and the ticket's metadata.
      inboundReq =
        XyneTypes.XyneInboundReq
          { channelId = cfg.channelId,
            threadId = threadId,
            subject = subject,
            body = createBodyText formFields body' contextFields,
            externalId = Nothing,
            senderName = Just senderName,
            senderEmail = Just senderEmail,
            additionalFormFields = Nothing
          }
  resp <-
    if null staged
      then XF.appDeskInboundAPI cfg.url token inboundReq
      else withStagedTempFiles staged $ \parts -> XF.appDeskInboundMultipartAPI cfg.url token inboundReq parts
  pure Common.RadarCreateTicketRes {ticketId = resp.ticketId, xyneId = resp.xyneId}

postRadarTicketsTicketUpdateStatus :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarUpdateStatusReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketUpdateStatus merchantShortId opCity ticketId req = do
  cfg <- resolveRadarCfg merchantShortId opCity
  stageName <- fromMaybeM (InvalidRequest "stageName is required.") (nonBlank req.stageName)
  token <- decrypt cfg.token
  -- Free-text stage, deliberately NOT Interface.updateTicketStatus: board
  -- stages are desk-configurable (BACKLOG / NOT REQUIRED / …) and the
  -- Interface variant only speaks the 5-value TicketStatus enum.
  void $
    XF.updateTicketStatusAPI
      cfg.url
      token
      XyneTypes.XyneUpdateTicketReq {ticketId = ticketId, channelId = cfg.channelId, stageName = stageName}
  pure APISuccess.Success

postRadarTicketsTicketCsat :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarCsatReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketCsat merchantShortId opCity ticketId req = do
  cfg <- resolveRadarCfg merchantShortId opCity
  -- Uses the row's own csatApiKey; absent key = logged no-op inside.
  XyneIf.updateTicketCsat cfg (Ticket.UpdateTicketCsatReq {xyneTicketId = ticketId, rating = req.rating, score = req.score, comment = req.comment})
  pure APISuccess.Success

-- * Request builders and helpers

requireSenderEmail :: Maybe Text -> Environment.Flow Text
requireSenderEmail mbEmail =
  -- Filled by the dashboard proxy from the verified session; absent means the
  -- session has no email, and desk writes must be attributable.
  fromMaybeM (InvalidRequest "Your session has no email; this action requires one.") (nonBlank =<< mbEmail)

mkListReq :: XyneCfg.XyneSpacesCfg -> Int -> Maybe Text -> Maybe XyneTypes.XyneListFilters -> Maybe Text -> XyneTypes.XyneListTicketsReq
mkListReq cfg pageLimit cursor filters senderEmail =
  XyneTypes.XyneListTicketsReq
    { channelId = Just cfg.channelId,
      projectId = Nothing,
      boardIds = Nothing,
      senderEmail = senderEmail,
      senderName = Nothing,
      filters = filters,
      customFields = Nothing,
      includeCustomFields = Nothing,
      limit = pageLimit,
      cursor = cursor
    }

-- | Query params arrive as free text; the wire type is enum-valued, so junk
-- gets a 400 here instead of an opaque upstream validation error.
parsePriority :: Text -> Environment.Flow XyneTypes.XynePriority
parsePriority t = case T.toUpper (T.strip t) of
  "LOW" -> pure XyneTypes.LOW
  "MEDIUM" -> pure XyneTypes.MEDIUM
  "HIGH" -> pure XyneTypes.HIGH
  "CRITICAL" -> pure XyneTypes.CRITICAL
  _ -> throwError $ InvalidRequest ("Invalid priority filter: " <> t <> ". Use LOW, MEDIUM, HIGH or CRITICAL.")

-- | The CC form's urgency → the desk's subject tag ([P1]…[P4]), preserving
-- the exact convention of the tickets the Express server created.
urgencyToTag :: Text -> Environment.Flow Text
urgencyToTag t = case T.toUpper (T.strip t) of
  "CRITICAL" -> pure "[P1]"
  "HIGH" -> pure "[P2]"
  "MEDIUM" -> pure "[P3]"
  "LOW" -> pure "[P4]"
  _ -> throwError $ InvalidRequest ("Invalid urgency: " <> t <> ". Use CRITICAL, HIGH, MEDIUM or LOW.")

mkFilters :: Maybe XyneTypes.XynePriority -> Maybe Text -> Maybe XyneTypes.XyneListFilters
mkFilters mbPriority mbStageName
  | isNothing mbPriority && isNothing mbStageName = Nothing
  | otherwise =
    Just
      XyneTypes.XyneListFilters
        { statusV2 = Nothing,
          priority = (: []) <$> mbPriority,
          stageName = (: []) <$> mbStageName,
          ticketType = Nothing,
          assignedTo = Nothing,
          createdBy = Nothing,
          userGroupId = Nothing,
          tags = Nothing,
          isArchived = Nothing,
          createdAfter = Nothing,
          createdBefore = Nothing
        }

-- | Cursor loop shared by summary and reporters: page through the channel,
-- hard-capped so one endpoint cannot spend unbounded upstream calls.
sweepTickets :: XyneCfg.XyneSpacesCfg -> Environment.Flow [XyneTypes.XyneTicketSummaryItem]
sweepTickets cfg = go Nothing [] 0 []
  where
    go cursor acc fetched seenCursors = do
      resp <- XyneIf.listTickets cfg (mkListReq cfg sweepPageSize cursor Nothing Nothing)
      let acc' = acc <> resp.items
          fetched' = fetched + length resp.items
          more = fromMaybe False resp.hasMore && isJust resp.nextCursor && not (null resp.items)
          -- Guard against a misbehaving upstream that returns hasMore=true with a
          -- repeating nextCursor: without this the loop appends duplicate pages
          -- until the cap, inflating summary tiles and squeezing out real tickets.
          repeated = maybe False (`elem` seenCursors) resp.nextCursor
      if more && not repeated && fetched' < summaryMaxTickets
        then go resp.nextCursor acc' fetched' (maybe seenCursors (: seenCursors) resp.nextCursor)
        else do
          when (more && repeated) $
            logWarning "radar sweep stopped early: upstream returned a repeating cursor - results are partial"
          when (more && not repeated && fetched' >= summaryMaxTickets) $
            logWarning ("radar summary truncated at " <> show summaryMaxTickets <> " tickets - counts are partial")
          pure acc'

resolveReporterEmail :: Text -> XyneCfg.XyneSpacesCfg -> Text -> Environment.Flow (Maybe Text)
resolveReporterEmail mocId cfg ticketId = do
  (mbCached :: Maybe Text) <- Redis.safeGet (reporterCacheKey mocId ticketId)
  case mbCached of
    Just e -> pure (Just e)
    Nothing -> do
      res <- try @_ @SomeException (XyneIf.getTicket cfg ticketId)
      case res of
        Right detail -> do
          let mbEmail = reporterFromDetail detail
          case mbEmail of
            Just e -> do
              Redis.setExp (reporterCacheKey mocId ticketId) e reporterCacheTtl
              pure (Just e)
            Nothing -> pure Nothing
        Left _ -> pure Nothing

reporterFromDetail :: XyneTypes.XyneTicketDetail -> Maybe Text
reporterFromDetail detail =
  detail.metadata >>= \m -> (nonBlank =<< m.reporterEmail) <|> (nonBlank =<< m.fromEmailAddress)

-- | Write the staged uploads to temp files for the multipart client, run the
-- action, and best-effort clean up.
withStagedTempFiles :: [StagedUpload] -> ([(Text, Text, Text, FilePath)] -> Environment.Flow a) -> Environment.Flow a
withStagedTempFiles staged action = do
  tmpDir <- liftIO getTemporaryDirectory
  parts <- forM staged $ \s -> do
    uid <- generateGUID
    let path = tmpDir <> "/radar-upload-" <> T.unpack uid
    bytes <- case B64.decode (TE.encodeUtf8 s.contentBase64) of
      Left err -> throwError $ InternalError ("Staged upload no longer decodes: " <> T.pack err)
      Right b -> pure b
    liftIO $ BS.writeFile path bytes
    pure ("files" :: Text, s.fileName, s.mimeType, path)
  result <- try @_ @SomeException (action parts)
  forM_ parts $ \(_, _, _, path) -> liftIO (removeFile path `catch` \(_ :: SomeException) -> pure ())
  either throwM pure result

-- * Mappers (field-for-field ports of the Express mapTicket/mapHistory/mapMessage)

iso8601 :: UTCTime -> Text
iso8601 = T.pack . iso8601Show

nowIsoText :: Environment.Flow Text
nowIsoText = iso8601 <$> getCurrentTime

-- | The Express @str()@: strings only count when non-blank after trimming.
nonBlank :: Text -> Maybe Text
nonBlank t = if T.null (T.strip t) then Nothing else Just t

mkTicketItem :: Text -> XyneTypes.XyneTicketSummaryItem -> Common.RadarTicketItem
mkTicketItem nowIso raw =
  Common.RadarTicketItem
    { id = fromMaybe "\8212" ((nonBlank =<< raw.ticketId) <|> (nonBlank =<< raw.xyneId)),
      xyneId = fromMaybe "\8212" (nonBlank =<< raw.xyneId),
      subject = fromMaybe "" (nonBlank =<< raw.title),
      priority = maybe "MEDIUM" T.toUpper (nonBlank =<< raw.priority),
      statusV2 = fromMaybe "TODO" (nonBlank =<< raw.statusV2),
      stageName = nonBlank =<< raw.stageName,
      createdAt = maybe nowIso iso8601 raw.createdAt,
      lastEmailAt = iso8601 <$> raw.lastEmailAt
    }

detailAsSummary :: XyneTypes.XyneTicketDetail -> XyneTypes.XyneTicketSummaryItem
detailAsSummary d =
  XyneTypes.XyneTicketSummaryItem
    { ticketId = d.id,
      xyneId = d.xyneId,
      title = d.title,
      statusV2 = d.statusV2,
      stageName = d.stageName,
      priority = d.priority,
      createdAt = d.createdAt,
      lastEmailAt = d.lastEmailAt,
      conversationId = d.conversationId,
      channelId = Nothing,
      boardId = Nothing,
      projectId = Nothing,
      customFormData = Nothing
    }

-- | History entries are heterogeneous per activity type, so they arrive as
-- raw JSON; pick the fields the dashboard renders and drop the rest.
mkEvent :: Int -> A.Value -> Common.RadarTicketEvent
mkEvent idx v =
  let actType = fromMaybe "ACTIVITY" (vString v "activityType")
   in Common.RadarTicketEvent
        { id = fromMaybe (show idx) (vString v "id"),
          eventType = actType,
          label = humanActivity actType,
          at = fromMaybe "" (vString v "timestamp"),
          actor = vObject v "updatedByUser" >>= \actor -> vString actor "name",
          from = humanValue (vObject v "value" >>= \val -> vString val "oldValue"),
          to = humanValue (vObject v "value" >>= \val -> vString val "newValue")
        }

mkMessage :: XyneTypes.XyneConversationMessage -> Common.RadarMessage
mkMessage m =
  Common.RadarMessage
    { id = fromMaybe "" (nonBlank =<< m.id),
      from = nonBlank =<< m.from,
      createdAt = iso8601 <$> m.createdAt,
      -- Raw HTML straight from email; the frontend sanitizes (DOMPurify).
      bodyHtml = fromMaybe "" m.body,
      attachments = maybe [] (map mkAttachment) m.attachments
    }

mkAttachment :: XyneTypes.XyneConversationAttachment -> Common.RadarAttachmentInfo
mkAttachment a =
  Common.RadarAttachmentInfo
    { id = fromMaybe "" (nonBlank =<< a.id),
      name = fromMaybe "attachment" (nonBlank =<< a.originalFilename),
      mimeType = nonBlank =<< a.mimetype,
      size = a.size
    }

-- * JSON helpers for the untyped history entries

vString :: A.Value -> Text -> Maybe Text
vString (A.Object o) k = case AKM.lookup (AKey.fromText k) o of
  Just (A.String s) | not (T.null (T.strip s)) -> Just s
  _ -> Nothing
vString _ _ = Nothing

vObject :: A.Value -> Text -> Maybe A.Value
vObject (A.Object o) k = AKM.lookup (AKey.fromText k) o
vObject _ _ = Nothing

-- * Text helpers

-- | \"[P1] [Control Center] Rides › Foo\" → \"Rides\"; text before the first
-- of @›@, @—@, @:@ after stripping the known subject prefixes.
categoryFromSubject :: Text -> Text
categoryFromSubject subject =
  let stripped = stripKnownPrefixes subject
      candidates = mapMaybe (breakBefore stripped) ["\8250", "\8212", ":"]
   in case candidates of
        (c : _) -> c
        [] -> let t = T.strip stripped in if T.null t then "Other" else t
  where
    breakBefore t sep =
      let (before, rest) = T.breakOn sep t
       in if T.null rest || T.null before then Nothing else Just (T.strip before)

stripKnownPrefixes :: Text -> Text
stripKnownPrefixes t0 =
  let t1 = T.stripStart t0
      t2 = T.stripStart (fromMaybe t1 (stripPriorityTag t1))
   in T.stripStart (fromMaybe t2 (stripControlCenterTag t2))
  where
    stripPriorityTag t = case T.unpack (T.take 4 t) of
      ['[', p, d, ']'] | Char.toUpper p == 'P' && Char.isDigit d -> Just (T.drop 4 t)
      _ -> Nothing
    stripControlCenterTag t =
      let tag = "[control center]"
       in if T.toLower (T.take (T.length tag) t) == tag then Just (T.drop (T.length tag) t) else Nothing

-- | \"ASSIGNED_TO\" → \"Assigned to\".
humanActivity :: Text -> Text
humanActivity t = capitalizeWord (T.replace "_" " " (T.toLower t))

capitalizeWord :: Text -> Text
capitalizeWord t = case T.uncons t of
  Just (c, rest) -> T.cons (Char.toUpper c) rest
  Nothing -> t

-- | Suppress raw cuid-like ids in history from/to values.
humanValue :: Maybe Text -> Maybe Text
humanValue = (>>= \t -> if isCuidLike t then Nothing else Just t)

isCuidLike :: Text -> Bool
isCuidLike t = case T.uncons t of
  Just ('c', rest) -> T.length rest >= 20 && T.all (\ch -> Char.isAsciiLower ch || Char.isDigit ch) rest
  _ -> False

-- | \"ravi.kumar_s@x.com\" → \"Ravi Kumar S\".
nameFromEmail :: Text -> Text
nameFromEmail email =
  let localPart = fromMaybe email (listToMaybe (T.splitOn "@" email))
      parts = filter (not . T.null) (T.split (\c -> c == '.' || c == '_' || c == '-') localPart)
   in T.unwords (map capitalizeWord parts)

-- | The create body, all plain text: @Key: value@ lines, then the operator's
-- description verbatim, then any auto-context lines. The RADAR board has no
-- metadata form (Xyne rejects @additionalFormFields@ with \"No form
-- configured\"), so these lines are the only place agents see the form
-- fields besides the subject. No markup is composed, so nothing needs
-- escaping. The reporter is NOT repeated here — senderName\/senderEmail on
-- the request already surface as the message's from and ticket metadata.
createBodyText :: [(Text, Text)] -> Text -> [(Text, Text)] -> Text
createBodyText formFields description contextFields =
  let fieldLine (k, v) = k <> ": " <> v
      block fields = T.unlines (map fieldLine (filter (not . T.null . snd) fields))
      contextBlock = if null contextFields then "" else "\n" <> block contextFields
   in block formFields <> "\n" <> description <> "\n" <> contextBlock

-- | S3 object-key classification for a rehosted attachment. The extension
-- only labels the key; the stored Content-Type is what the presigned
-- download serves.
mimeToFileTypeExt :: Maybe Text -> (S3.FileType, Text)
mimeToFileTypeExt mbMt =
  let mt = maybe "" (T.toLower . T.strip . T.takeWhile (/= ';')) mbMt
      fileType
        | "image/" `T.isPrefixOf` mt = S3.Image
        | "audio/" `T.isPrefixOf` mt = S3.Audio
        | "video/" `T.isPrefixOf` mt = S3.Video
        | otherwise = S3.PDF
      ext = case T.splitOn "/" mt of
        [_, sub]
          | sub == "jpeg" -> ".jpg"
          | sub == "mpeg" -> ".mp3"
          | sub == "svg+xml" -> ".svg"
          | not (T.null sub) -> "." <> sub
        _ -> ".bin"
   in (fileType, ext)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b
