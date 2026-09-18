{-# OPTIONS_GHC -Wwarn=unused-imports #-}

-- | RADAR ticket desk, backed by Xyne Spaces. Dashboard-facing APIs over the
-- Control Center support channel. The read\/reply endpoints are ported 1:1
-- from the control-center Express implementation
-- (server\/src\/routes\/radarTickets.ts); behavioural constants (page caps,
-- sweep cap, reply limits) are kept identical so the frontend needs no
-- changes beyond the repoint. Create\/updateStatus\/csat\/conversation expose
-- the rest of the Xyne client surface.
--
-- Deliberate deviations from the Express version:
--
--   * Message @bodyHtml@ goes out raw — sanitization moves to the frontend
--     (DOMPurify), since no HTML-allowlist sanitizer exists here and
--     stripping tags would lose the desk formatting.
--   * Attachments are fetched from Xyne once, rehosted to our S3 and served
--     as a presigned URL, instead of stream-proxying: 'callAPI' JSON-logs
--     response bodies and binary through the two-hop dashboard client is
--     unproven.
--   * The reporter cache lives in Redis (30d) instead of process memory.
--   * Create cannot forward @priority@ yet: the JSON 'Xyne.XyneInboundReq'
--     has no priority field (the Express create sent it as a multipart form
--     field). Follow-up: add @priority :: Maybe Text@ to the shared-kernel
--     type, then thread @req.priority@ through here.
module Domain.Action.Dashboard.Management.RadarTickets
  ( getRadarTicketsList,
    getRadarTicketsSummary,
    getRadarTicketsReporters,
    getRadarTicketsTicket,
    getRadarTicketsTicketAttachment,
    getRadarTicketsTicketConversation,
    postRadarTicketsTicketReply,
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Utils as DU
import qualified Environment
import qualified Kernel.External.Ticket.XyneSpaces.Flow as Xyne
import qualified Kernel.External.Ticket.XyneSpaces.Types as Xyne
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)

-- Constants carried over verbatim from the control-center implementation.
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

reporterCacheKey :: Text -> Text
reporterCacheKey ticketId = "radar:xyne:reporter:" <> ticketId

attachmentCacheKey :: Text -> Text
attachmentCacheKey attachmentId = "radar:xyne:attachment:s3:" <> attachmentId

getRadarCfg :: Environment.Flow Environment.RadarXyneCfg
getRadarCfg = do
  cfg <- asks (.radarXyneCfg)
  when (T.null (T.strip cfg.appJwt) || T.null (T.strip cfg.deskChannelId)) $
    throwError $ InvalidRequest "Ticketing is not configured for this environment."
  pure cfg

getRadarTicketsList :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Environment.Flow Common.RadarTicketListRes
getRadarTicketsList _merchantShortId _opCity mbPageSize mbCursor mbPriority mbStageName mbReporterEmail = do
  cfg <- getRadarCfg
  mbPriorityEnum <- mapM parsePriority (nonBlank =<< mbPriority)
  let pageSize = min maxPageSize (max 1 (fromMaybe 25 mbPageSize))
      mbFilters = mkFilters mbPriorityEnum (nonBlank =<< mbStageName)
  resp <- Xyne.listTicketsAPI cfg.baseUrl cfg.appJwt (mkListReq cfg pageSize (nonBlank =<< mbCursor) mbFilters (nonBlank =<< mbReporterEmail))
  nowIso <- nowIsoText
  pure
    Common.RadarTicketListRes
      { items = map (mkTicketItem nowIso) resp.items,
        hasMore = fromMaybe False resp.hasMore,
        nextCursor = resp.nextCursor
      }

getRadarTicketsSummary :: ShortId DM.Merchant -> Context.City -> Environment.Flow Common.RadarSummaryRes
getRadarTicketsSummary _merchantShortId _opCity = do
  cfg <- getRadarCfg
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
getRadarTicketsReporters _merchantShortId _opCity = do
  cfg <- getRadarCfg
  rawItems <- sweepTickets cfg
  let ticketIds = List.nub (mapMaybe (\item -> nonBlank =<< item.ticketId) rawItems)
  -- The list response carries no sender, so each uncached reporter costs a
  -- detail fetch. Batched concurrency keeps a cold cache from serialising
  -- hundreds of upstream calls; per-ticket failures are swallowed so one
  -- unreadable ticket cannot empty the dropdown.
  emails <- fmap (catMaybes . concat) $
    forM (chunksOf reporterBatchSize ticketIds) $ \batch ->
      DU.mapConcurrently (resolveReporterEmail cfg) batch
  let reporters = List.sortOn (.name) [Common.RadarReporter {email = e, name = nameFromEmail e} | e <- List.nub emails]
  pure Common.RadarReportersRes {reporters = reporters}

getRadarTicketsTicket :: ShortId DM.Merchant -> Context.City -> Text -> Environment.Flow Common.RadarTicketDetailRes
getRadarTicketsTicket _merchantShortId _opCity ticketId = do
  cfg <- getRadarCfg
  detail <- Xyne.getTicketAPI cfg.baseUrl cfg.appJwt ticketId >>= fromMaybeM (InvalidRequest "Ticket not found.")
  -- Conversation failure degrades to an empty thread rather than failing the
  -- whole detail view (matches the Express behaviour).
  convRes <- try @_ @SomeException (Xyne.getConversationAPI cfg.baseUrl cfg.appJwt ticketId)
  let conv = case convRes of
        Right (Just c) -> c
        _ -> Xyne.XyneConversationResp {items = [], hasMore = Nothing}
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
        updatedAt = nonBlank =<< detail.updatedAt,
        reporterEmail = reporterFromDetail detail,
        emailCount = detail.emailCount,
        history = maybe [] (zipWith mkEvent [0 ..]) detail.history,
        messages = map mkMessage conv.items
      }

getRadarTicketsTicketConversation :: ShortId DM.Merchant -> Context.City -> Text -> Environment.Flow Common.RadarConversationRes
getRadarTicketsTicketConversation _merchantShortId _opCity ticketId = do
  cfg <- getRadarCfg
  conv <- Xyne.getConversationAPI cfg.baseUrl cfg.appJwt ticketId >>= fromMaybeM (InvalidRequest "Ticket not found.")
  pure Common.RadarConversationRes {messages = map mkMessage conv.items, hasMore = fromMaybe False conv.hasMore}

getRadarTicketsTicketAttachment :: ShortId DM.Merchant -> Context.City -> Text -> Text -> Environment.Flow Common.RadarAttachmentUrlRes
getRadarTicketsTicketAttachment _merchantShortId _opCity _ticketId attachmentId = do
  cfg <- getRadarCfg
  (mbCachedPath :: Maybe Text) <- Redis.safeGet (attachmentCacheKey attachmentId)
  filePath <- case mbCachedPath of
    Just p -> pure p
    Nothing -> do
      (bytes, mbContentType) <- Xyne.downloadFileAPI cfg.baseUrl cfg.appJwt attachmentId >>= fromMaybeM (InvalidRequest "Attachment not found.")
      let (fileType, ext) = mimeToFileTypeExt mbContentType
          contentType = fromMaybe "application/octet-stream" (nonBlank =<< mbContentType)
      newPath <- S3.createFilePath "radar-attachments/" ("xyne-" <> attachmentId) fileType ext
      S3.putRaw (T.unpack newPath) (LBS.toStrict bytes) (T.unpack contentType)
      Redis.setExp (attachmentCacheKey attachmentId) newPath attachmentCacheTtl
      pure newPath
  url <- S3.generateDownloadUrl (T.unpack filePath) attachmentUrlExpiry
  pure Common.RadarAttachmentUrlRes {url = url}

postRadarTicketsTicketReply :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarReplyReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketReply _merchantShortId _opCity ticketId req = do
  cfg <- getRadarCfg
  senderEmail <- requireSenderEmail req.senderEmail
  let body' = T.strip req.body
  when (T.null body') $ throwError $ InvalidRequest "Write a message before sending."
  when (T.length body' > maxReplyChars) $ throwError $ InvalidRequest ("Keep the message under " <> show maxReplyChars <> " characters.")
  checkSlidingWindowLimitWithOptions ("radar:reply:rateLimit:" <> senderEmail) writeRateLimitOptions
  detail <- Xyne.getTicketAPI cfg.baseUrl cfg.appJwt ticketId >>= fromMaybeM (InvalidRequest "Ticket not found.")
  conv <- Xyne.getConversationAPI cfg.baseUrl cfg.appJwt ticketId >>= fromMaybeM (InvalidRequest "Ticket not found.")
  -- The reply thread key is the externalThreadId of the FIRST message; only
  -- tickets filed from Control Center carry one.
  firstMsg <- fromMaybeM notFiledFromCC (listToMaybe conv.items)
  threadId <- fromMaybeM notFiledFromCC (nonBlank =<< firstMsg.externalThreadId)
  uuid <- generateGUID
  let subject = fromMaybe (fromMaybe "" (nonBlank =<< detail.title)) (nonBlank =<< firstMsg.subject)
      inboundReq =
        Xyne.XyneInboundReq
          { channelId = cfg.deskChannelId,
            threadId = threadId,
            subject = subject,
            body = replyBodyHtml body',
            externalId = Just (threadId <> "-r-" <> uuid),
            senderName = (nonBlank =<< req.senderName) <|> Just senderEmail,
            senderEmail = Just senderEmail,
            additionalFormFields = Nothing
          }
  resp <- Xyne.appDeskInboundAPI cfg.baseUrl cfg.appJwt inboundReq
  -- isNew means the desk opened a brand-new ticket instead of appending to
  -- this one — surface it loudly rather than silently forking the thread.
  when resp.isNew $
    throwError $ InternalError $ "Xyne opened a new ticket (" <> resp.xyneId <> ") instead of replying on " <> ticketId
  pure APISuccess.Success
  where
    notFiledFromCC = InvalidRequest "This ticket wasn't filed from Control Center, so replies must happen on the Xyne desk."

postRadarTicketsCreate :: ShortId DM.Merchant -> Context.City -> Common.RadarCreateTicketReq -> Environment.Flow Common.RadarCreateTicketRes
postRadarTicketsCreate _merchantShortId _opCity req = do
  cfg <- getRadarCfg
  senderEmail <- requireSenderEmail req.senderEmail
  subject <- fromMaybeM (InvalidRequest "Give the ticket a subject.") (nonBlank req.subject)
  let body' = T.strip req.body
  when (T.null body') $ throwError $ InvalidRequest "Write a message before sending."
  when (T.length body' > maxReplyChars) $ throwError $ InvalidRequest ("Keep the message under " <> show maxReplyChars <> " characters.")
  checkSlidingWindowLimitWithOptions ("radar:create:rateLimit:" <> senderEmail) writeRateLimitOptions
  uuid <- generateGUID
  let threadId = "radar-" <> uuid
      inboundReq =
        Xyne.XyneInboundReq
          { channelId = cfg.deskChannelId,
            threadId = threadId,
            subject = subject,
            body = replyBodyHtml body',
            externalId = Just threadId,
            senderName = (nonBlank =<< req.senderName) <|> Just senderEmail,
            senderEmail = Just senderEmail,
            additionalFormFields = Nothing
          }
  resp <- Xyne.appDeskInboundAPI cfg.baseUrl cfg.appJwt inboundReq
  pure Common.RadarCreateTicketRes {ticketId = resp.ticketId, xyneId = resp.xyneId}

postRadarTicketsTicketUpdateStatus :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarUpdateStatusReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketUpdateStatus _merchantShortId _opCity ticketId req = do
  cfg <- getRadarCfg
  stageName <- fromMaybeM (InvalidRequest "stageName is required.") (nonBlank req.stageName)
  void $
    Xyne.updateTicketStatusAPI
      cfg.baseUrl
      cfg.appJwt
      Xyne.XyneUpdateTicketReq {ticketId = ticketId, channelId = cfg.deskChannelId, stageName = stageName}
  pure APISuccess.Success

postRadarTicketsTicketCsat :: ShortId DM.Merchant -> Context.City -> Text -> Common.RadarCsatReq -> Environment.Flow APISuccess.APISuccess
postRadarTicketsTicketCsat _merchantShortId _opCity ticketId req = do
  cfg <- getRadarCfg
  -- CSAT authenticates with its own X-Api-Key, not the app JWT.
  when (T.null (T.strip cfg.csatApiKey)) $
    throwError $ InvalidRequest "CSAT is not configured for this environment."
  void $
    Xyne.updateCsatAPI
      cfg.baseUrl
      cfg.csatApiKey
      ticketId
      Xyne.XyneCsatReq {rating = req.rating, score = req.score, comment = req.comment}
  pure APISuccess.Success

-- * Xyne request builders

requireSenderEmail :: Maybe Text -> Environment.Flow Text
requireSenderEmail mbEmail =
  -- Filled by the dashboard proxy from the verified session; absent means the
  -- session has no email, and desk writes must be attributable.
  fromMaybeM (InvalidRequest "Your session has no email; this action requires one.") (nonBlank =<< mbEmail)

mkListReq :: Environment.RadarXyneCfg -> Int -> Maybe Text -> Maybe Xyne.XyneListFilters -> Maybe Text -> Xyne.XyneListTicketsReq
mkListReq cfg pageLimit cursor filters senderEmail =
  Xyne.XyneListTicketsReq
    { channelId = Just cfg.deskChannelId,
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

-- | Query params arrive as free text; the wire type is enum-valued now, so
-- junk gets a 400 here instead of an opaque upstream validation error.
parsePriority :: Text -> Environment.Flow Xyne.XynePriority
parsePriority t = case T.toUpper (T.strip t) of
  "LOW" -> pure Xyne.LOW
  "MEDIUM" -> pure Xyne.MEDIUM
  "HIGH" -> pure Xyne.HIGH
  "CRITICAL" -> pure Xyne.CRITICAL
  _ -> throwError $ InvalidRequest ("Invalid priority filter: " <> t <> ". Use LOW, MEDIUM, HIGH or CRITICAL.")

mkFilters :: Maybe Xyne.XynePriority -> Maybe Text -> Maybe Xyne.XyneListFilters
mkFilters mbPriority mbStageName
  | isNothing mbPriority && isNothing mbStageName = Nothing
  | otherwise =
    Just
      Xyne.XyneListFilters
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
sweepTickets :: Environment.RadarXyneCfg -> Environment.Flow [Xyne.XyneTicketSummaryItem]
sweepTickets cfg = go Nothing [] 0
  where
    go cursor acc fetched = do
      resp <- Xyne.listTicketsAPI cfg.baseUrl cfg.appJwt (mkListReq cfg sweepPageSize cursor Nothing Nothing)
      let acc' = acc <> resp.items
          fetched' = fetched + length resp.items
          more = fromMaybe False resp.hasMore && isJust resp.nextCursor && not (null resp.items)
      if more && fetched' < summaryMaxTickets
        then go resp.nextCursor acc' fetched'
        else do
          when (more && fetched' >= summaryMaxTickets) $
            logWarning $ "radar summary truncated at " <> show summaryMaxTickets <> " tickets - counts are partial"
          pure acc'

resolveReporterEmail :: Environment.RadarXyneCfg -> Text -> Environment.Flow (Maybe Text)
resolveReporterEmail cfg ticketId = do
  (mbCached :: Maybe Text) <- Redis.safeGet (reporterCacheKey ticketId)
  case mbCached of
    Just e -> pure (Just e)
    Nothing -> do
      res <- try @_ @SomeException (Xyne.getTicketAPI cfg.baseUrl cfg.appJwt ticketId)
      case res of
        Right (Just detail) -> do
          let mbEmail = reporterFromDetail detail
          case mbEmail of
            Just e -> do
              Redis.setExp (reporterCacheKey ticketId) e reporterCacheTtl
              pure (Just e)
            Nothing -> pure Nothing
        _ -> pure Nothing

reporterFromDetail :: Xyne.XyneTicketDetail -> Maybe Text
reporterFromDetail detail =
  detail.metadata >>= \m -> (nonBlank =<< m.reporterEmail) <|> (nonBlank =<< m.fromEmailAddress)

-- * Mappers (field-for-field ports of the Express mapTicket/mapHistory/mapMessage)

mkTicketItem :: Text -> Xyne.XyneTicketSummaryItem -> Common.RadarTicketItem
mkTicketItem nowIso raw =
  Common.RadarTicketItem
    { id = fromMaybe "\8212" ((nonBlank =<< raw.ticketId) <|> (nonBlank =<< raw.xyneId)),
      xyneId = fromMaybe "\8212" (nonBlank =<< raw.xyneId),
      subject = fromMaybe "" (nonBlank =<< raw.title),
      priority = maybe "MEDIUM" T.toUpper (nonBlank =<< raw.priority),
      statusV2 = fromMaybe "TODO" (nonBlank =<< raw.statusV2),
      stageName = nonBlank =<< raw.stageName,
      createdAt = fromMaybe nowIso (nonBlank =<< raw.createdAt),
      lastEmailAt = nonBlank =<< raw.lastEmailAt
    }

detailAsSummary :: Xyne.XyneTicketDetail -> Xyne.XyneTicketSummaryItem
detailAsSummary d =
  Xyne.XyneTicketSummaryItem
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

mkMessage :: Xyne.XyneConversationMessage -> Common.RadarMessage
mkMessage m =
  Common.RadarMessage
    { id = fromMaybe "" (nonBlank =<< m.id),
      from = nonBlank =<< m.from,
      createdAt = nonBlank =<< m.createdAt,
      -- Raw HTML straight from email; the frontend sanitizes (DOMPurify).
      bodyHtml = fromMaybe "" m.body,
      attachments = maybe [] (map mkAttachment) m.attachments
    }

mkAttachment :: Xyne.XyneConversationAttachment -> Common.RadarAttachmentInfo
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

-- * Small helpers

nowIsoText :: Environment.Flow Text
nowIsoText = T.pack . iso8601Show <$> getCurrentTime

-- | The Express @str()@: strings only count when non-blank after trimming.
nonBlank :: Text -> Maybe Text
nonBlank t = if T.null (T.strip t) then Nothing else Just t

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

-- | Plain text → minimal HTML: blank-line-separated paragraphs, single
-- newlines become @\<br\>@, everything HTML-escaped.
replyBodyHtml :: Text -> Text
replyBodyHtml text =
  T.concat (map renderParagraph (splitParagraphs text))
  where
    renderParagraph p = "<p>" <> T.intercalate "<br>" (T.splitOn "\n" (escapeHtml (T.strip p))) <> "</p>"

splitParagraphs :: Text -> [Text]
splitParagraphs t = go (T.lines t) [] []
  where
    go [] current acc = reverse (flush current acc)
    go (l : ls) current acc
      | T.null (T.strip l) = go ls [] (flush current acc)
      | otherwise = go ls (l : current) acc
    flush [] acc = acc
    flush current acc = T.intercalate "\n" (reverse current) : acc

escapeHtml :: Text -> Text
escapeHtml =
  T.replace "'" "&#39;"
    . T.replace "\"" "&quot;"
    . T.replace ">" "&gt;"
    . T.replace "<" "&lt;"
    . T.replace "&" "&amp;"

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
