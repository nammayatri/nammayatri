{-# LANGUAGE PackageImports #-}

module Lib.Finance.Settlement.Sources.Email
  ( fetchSettlementFile,
    fetchSettlementFileWithPlainPassword,
    fetchSettlementFileDebugRaw,
    extractMessageIds,
    extractCsvFromMime,
  )
where

import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (EmailConfig (..))
import Kernel.Prelude
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import qualified Prelude as P

fetchSettlementFile ::
  (EncFlow m r, MonadIO m) =>
  EmailConfig ->
  m (Either Text LBS.ByteString)
fetchSettlementFile config = do
  decryptedPassword <- decrypt config.password
  fetchSettlementFileWithPlainPassword config decryptedPassword

fetchSettlementFileWithPlainPassword ::
  (MonadIO m) =>
  EmailConfig ->
  Text ->
  m (Either Text LBS.ByteString)
fetchSettlementFileWithPlainPassword config plainPassword = do
  let imapUrl =
        "imaps://" <> T.unpack config.imapHost <> ":" <> P.show config.imapPort
          <> "/"
          <> T.unpack config.folderName
      searchQuery = maybe "UNSEEN" (\s -> "SUBJECT \"" <> T.unpack s <> "\" UNSEEN") config.subjectFilter
  (exitCode, stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "curl"
        [ "--silent",
          "--url",
          imapUrl,
          "--user",
          T.unpack config.username <> ":" <> T.unpack plainPassword,
          "-X",
          "SEARCH " <> searchQuery
        ]
        ""
  case exitCode of
    ExitFailure code ->
      pure $ Left $ "IMAP SEARCH failed (exit " <> show code <> "): " <> T.pack stderr
    ExitSuccess -> do
      let msgIds = extractMessageIds (T.pack stdout)
      case msgIds of
        [] -> pure $ Left "No unread settlement emails found"
        (latestId : _) -> fetchAndExtractAttachment config plainPassword latestId

fetchAndExtractAttachment ::
  (MonadIO m) =>
  EmailConfig ->
  Text ->
  Text ->
  m (Either Text LBS.ByteString)
fetchAndExtractAttachment config password msgId = do
  let fetchUrl =
        "imaps://" <> T.unpack config.imapHost <> ":" <> P.show config.imapPort
          <> "/"
          <> T.unpack config.folderName
          <> "/;UID="
          <> T.unpack msgId
  (exitCode, stdout, stderr) <-
    liftIO $
      readProcessWithExitCode
        "curl"
        [ "--silent",
          "--url",
          fetchUrl,
          "--user",
          T.unpack config.username <> ":" <> T.unpack password
        ]
        ""
  case exitCode of
    ExitFailure code ->
      pure $ Left $ "IMAP FETCH failed (exit " <> show code <> "): " <> T.pack stderr
    ExitSuccess ->
      pure $ extractCsvFromMime (T.pack stdout)

fetchSettlementFileDebugRaw ::
  (MonadIO m) =>
  EmailConfig ->
  Text ->
  m (Maybe Text)
fetchSettlementFileDebugRaw config plainPassword = do
  let imapUrl =
        "imaps://" <> T.unpack config.imapHost <> ":" <> P.show config.imapPort
          <> "/"
          <> T.unpack config.folderName
      searchQuery = maybe "UNSEEN" (\s -> "SUBJECT \"" <> T.unpack s <> "\" UNSEEN") config.subjectFilter
  (exitCode, stdout, _) <-
    liftIO $
      readProcessWithExitCode
        "curl"
        [ "--silent",
          "--url",
          imapUrl,
          "--user",
          T.unpack config.username <> ":" <> T.unpack plainPassword,
          "-X",
          "SEARCH " <> searchQuery
        ]
        ""
  case exitCode of
    ExitFailure _ -> pure Nothing
    ExitSuccess -> do
      let msgIds = extractMessageIds (T.pack stdout)
      case msgIds of
        [] -> pure Nothing
        (latestId : _) -> do
          let fetchUrl =
                "imaps://" <> T.unpack config.imapHost <> ":" <> P.show config.imapPort
                  <> "/"
                  <> T.unpack config.folderName
                  <> "/;UID="
                  <> T.unpack latestId
          (exitCode2, stdout2, _) <-
            liftIO $
              readProcessWithExitCode
                "curl"
                [ "--silent",
                  "--url",
                  fetchUrl,
                  "--user",
                  T.unpack config.username <> ":" <> T.unpack plainPassword
                ]
                ""
          case exitCode2 of
            ExitFailure _ -> pure Nothing
            ExitSuccess -> pure $ Just (T.pack stdout2)

extractMessageIds :: Text -> [Text]
extractMessageIds raw =
  let parts = T.words raw
   in reverse $ filter (T.all (\c -> c >= '0' && c <= '9')) parts

extractCsvFromMime :: Text -> Either Text LBS.ByteString
extractCsvFromMime mimeText =
  let linesList = map (T.dropWhileEnd (== '\r')) (T.lines mimeText)
      csvBoundaries = findCsvSection linesList
   in case csvBoundaries of
        Nothing -> Left "No CSV attachment found in email"
        Just content -> decodeContent content

findCsvSection :: [Text] -> Maybe [Text]
findCsvSection [] = Nothing
findCsvSection (line : rest)
  | isCsvContentType line =
    let bodyLines = dropWhile (not . T.null) rest
     in Just $ takeWhile (not . isBoundaryLine) (drop 1 bodyLines)
  | otherwise = findCsvSection rest
  where
    isCsvContentType l =
      let lower = T.toLower l
       in "content-type:" `T.isInfixOf` lower && ("text/csv" `T.isInfixOf` lower || "application/csv" `T.isInfixOf` lower)
    isBoundaryLine l = "--" `T.isPrefixOf` l

decodeContent :: [Text] -> Either Text LBS.ByteString
decodeContent contentLines =
  let joined = T.unlines contentLines
      stripped = T.strip joined
   in case B64.decode (TE.encodeUtf8 $ T.filter (/= '\n') stripped) of
        Right decoded -> Right $ LBS.fromStrict decoded
        Left _ -> Right $ LBS.fromStrict $ TE.encodeUtf8 stripped
