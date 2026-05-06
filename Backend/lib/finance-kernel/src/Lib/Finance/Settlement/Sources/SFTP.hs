{-# LANGUAGE RecordWildCards #-}

module Lib.Finance.Settlement.Sources.SFTP
  ( fetchSettlementFile,
    SftpFetchMeta (..),
  )
where

import Codec.Archive.Zip (Archive (..), Entry (..), fromEntry, toArchive)
import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified EulerHS.Language as L
import Kernel.External.Encryption
import Kernel.External.Settlement.Types (SFTPConfig (..), SettlementParserTypeMap, SplitSettlementCustomerType)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import Kernel.Utils.Common (logInfo)
import Kernel.Utils.Logging (logDebug)
import Kernel.Utils.Servant.Client (HasRequestId)
import Lib.Finance.Domain.Types.SettlementFileInfo (SettlementFileInfo (..), SettlementFileStatus (..))
import Lib.Finance.Settlement.ParserTypeMap (parserMapAcceptsRemoteName, resolveSplitCustomerType)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.SettlementFileInfo as QSFI
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)
import qualified Prelude as P

data SftpFetchMeta = SftpFetchMeta
  { trackedFileId :: Id SettlementFileInfo,
    firstDataRowIndex :: Int,
    dataRowsDelivered :: Int
  }
  deriving stock (Show, Generic)

fetchSettlementFile ::
  ( BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Text ->
  Maybe SettlementParserTypeMap ->
  SFTPConfig ->
  m (Either Text (LBS.ByteString, SftpFetchMeta, SplitSettlementCustomerType))
fetchSettlementFile merchantId merchantOperatingCityId paymentGatewayName mbParserTypeMap config = do
  pendingRows <-
    QSFI.findAllByPaymentGatewayMerchantCityAndStatus
      paymentGatewayName
      merchantId
      merchantOperatingCityId
      PENDING
  case List.sortOn (.fileName) pendingRows of
    (row : _) -> do
      logInfo $
        "fetchSettlementFile: PENDING resume fast-path file=" <> row.fileName
          <> " trackedFileId=" <> Kernel.Types.Id.getId row.id
          <> " lastProcessedIndex=" <> T.pack (show row.lastProcessedIndex)
      case guardRemoteName row.fileName of
        Left err -> pure $ Left err
        Right norm ->
          downloadAndSlice
            config
            mbParserTypeMap
            norm
            row.id
            (row.lastProcessedIndex + 1)
            config.csvChunkRowLimit
    [] -> do
      logInfo "fetchSettlementFile: no PENDING resume; running DFS fresh-discovery"
      freshDiscovery
  where
    freshDiscovery = do
      eFile <- findFirstUnprocessedRemoteFile config paymentGatewayName mbParserTypeMap
      case eFile of
        Left err -> pure $ Left err
        Right Nothing ->
          pure $
            Left
              "No SFTP file to ingest: DFS found no parserTypeMap-matching file that is not already tracked in settlement_file_info"
        Right (Just relPath) ->
          case guardRemoteName relPath of
            Left err -> pure $ Left err
            Right norm -> do
              now <- getCurrentTime
              newId <- generateGUID
              let sfId = Id newId
                  row =
                    SettlementFileInfo
                      { id = sfId,
                        paymentGatewayName,
                        fileName = norm,
                        status = PENDING,
                        lastProcessedIndex = -1,
                        merchantId,
                        merchantOperatingCityId,
                        createdAt = now,
                        updatedAt = now
                      }
              QSFI.create row
              logInfo $
                "fetchSettlementFile: fresh-discovery created PENDING row trackedFileId="
                  <> Kernel.Types.Id.getId sfId
                  <> " fileName=" <> norm
              downloadAndSlice config mbParserTypeMap norm sfId 0 config.csvChunkRowLimit

downloadAndSlice ::
  ( EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SFTPConfig ->
  Maybe SettlementParserTypeMap ->
  Text ->
  Id SettlementFileInfo ->
  Int ->
  Maybe Int ->
  m (Either Text (LBS.ByteString, SftpFetchMeta, SplitSettlementCustomerType))
downloadAndSlice cfg mbParserTypeMap remoteFile infoId startRow chunkLimit = do
  eBytes <- downloadRemoteBytes cfg remoteFile
  case eBytes of
    Left err -> pure $ Left err
    Right bytes -> do
      let csvBytes =
            if ".zip" `List.isSuffixOf` map Char.toLower (T.unpack remoteFile)
              then unzipFirstCsv bytes
              else bytes
      case sliceCsvPayload startRow chunkLimit csvBytes of
        Left err -> pure $ Left err
        Right (sliced, nData) -> do
          let splitTy = resolveSplitCustomerType mbParserTypeMap remoteFile
              meta =
                SftpFetchMeta
                  { trackedFileId = infoId,
                    firstDataRowIndex = startRow,
                    dataRowsDelivered = nData
                  }
          logDebug $
            "downloadAndSlice: file=" <> show remoteFile
              <> " startRow=" <> show startRow
              <> " chunkLimit=" <> show chunkLimit
              <> " nData=" <> show nData
              <> " splitTy=" <> show splitTy
          pure $ Right (sliced, meta, splitTy)

-- | DFS through the remote SFTP tree, returning the *first* file path that:
--
--   1. Has a settlement suffix (@.csv@, @.zip@, @.csv.zip@), AND
--   2. Matches @parserTypeMap@ basenames (or is accepted when no map is set), AND
--   3. Is not yet tracked in @settlement_file_info@ for this gateway.
--
-- Within each directory, files are checked before subdirectories are descended,
-- both sorted alphabetically. One @sftp@ invocation per directory we actually
-- visit — early termination stops at the first hit. Returned path is relative
-- to @remotePath@ / SFTP home, e.g.
-- @"CUMTA/2026/01/01/UPI_YP_SR_..._00382262.csv.zip"@.
findFirstUnprocessedRemoteFile ::
  ( BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SFTPConfig ->
  Text -> -- paymentGatewayName
  Maybe SettlementParserTypeMap ->
  m (Either Text (Maybe Text))
findFirstUnprocessedRemoteFile cfg paymentGatewayName mbParserTypeMap = do
  let initialDir = case cfg.remotePath of
        Just p | not (T.null p) ->
          let trimmed = T.dropWhileEnd (== '/') p
           in if T.null trimmed then "/" else T.unpack trimmed
        _ -> "."
  logDebug $ "findFirstUnprocessedRemoteFile: initialDir=" <> show initialDir
  eFirst <- runSftpListBatch cfg [initialDir]
  case eFirst of
    Left err -> do
      logDebug $ "findFirstUnprocessedRemoteFile: initial listing failed: " <> err
      pure $ Left err
    Right [] -> do
      logDebug "findFirstUnprocessedRemoteFile: initial listing returned no blocks"
      pure $ Right Nothing
    Right ((rootAbs, rootEntries) : _) -> do
      logDebug $
        "findFirstUnprocessedRemoteFile: rootAbs=" <> show rootAbs
          <> " rootEntries=" <> show (length rootEntries)
      eFound <-
        dfsFindUnprocessed
          cfg
          paymentGatewayName
          mbParserTypeMap
          rootAbs
          rootAbs
          rootEntries
      case eFound of
        Left err -> pure $ Left err
        Right Nothing -> do
          logInfo "findFirstUnprocessedRemoteFile: DFS exhausted, no unprocessed file found"
          pure $ Right Nothing
        Right (Just absPath) ->
          pure $ Right (Just (T.pack (relativizeTo rootAbs absPath)))

-- | Pure DFS step. Files in @entries@ are checked first (sorted alphabetically);
-- if none qualifies, subdirectories are descended in the same order. Each
-- descent issues exactly one @runSftpListBatch@ for that subdir.
dfsFindUnprocessed ::
  ( BeamFlow m r,
    EncFlow m r,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  SFTPConfig ->
  Text ->
  Maybe SettlementParserTypeMap ->
  String -> -- rootAbs (kept across recursion for relativization)
  String -> -- currentAbs
  [(Bool, String)] ->
  m (Either Text (Maybe String))
dfsFindUnprocessed cfg gw mbMap rootAbs currentAbs entries =
  go (List.sort entries)
  where
    -- (False, _) sorts before (True, _) → files checked before subdirs descended.
    go [] = pure $ Right Nothing
    go ((False, name) : rest) = checkFile name rest
    go ((True, name) : rest) = descendInto name rest

    checkFile name rest = do
      let absPath = joinAbs currentAbs name
      if matchesSettlementSuffix name && parserMapAcceptsRemoteName mbMap (T.pack name)
        then do
          let relPath = T.pack (relativizeTo rootAbs absPath)
          mbExisting <- QSFI.findByPaymentGatewayNameAndFileName gw relPath
          case mbExisting of
            Just r -> do
              logDebug $
                "dfsFindUnprocessed: tracked, skipping. file=" <> relPath
                  <> " existingId=" <> Kernel.Types.Id.getId r.id
                  <> " existingStatus=" <> T.pack (show r.status)
              go rest
            Nothing -> do
              logInfo $ "dfsFindUnprocessed: found unprocessed candidate file=" <> relPath
              pure $ Right (Just absPath)
        else go rest

    descendInto name rest = do
      let subAbs = joinAbs currentAbs name
      logDebug $ "dfsFindUnprocessed: descending subAbs=" <> show subAbs
      eSub <- runSftpListBatch cfg [subAbs]
      case eSub of
        Left err -> pure $ Left err
        Right [] -> go rest
        Right ((_, subEntries) : _) -> do
          eFound <- dfsFindUnprocessed cfg gw mbMap rootAbs subAbs subEntries
          case eFound of
            Left err -> pure $ Left err
            Right (Just f) -> pure $ Right (Just f)
            Right Nothing -> go rest

joinAbs :: String -> String -> String
joinAbs base name
  | null base = name
  | "/" `List.isSuffixOf` base = base ++ name
  | otherwise = base ++ "/" ++ name

relativizeTo :: String -> String -> String
relativizeTo base p =
  case List.stripPrefix base p of
    Just rest -> dropWhile (== '/') rest
    Nothing -> p

matchesSettlementSuffix :: String -> Bool
matchesSettlementSuffix n =
  let l = map Char.toLower n
   in ".csv" `List.isSuffixOf` l || ".zip" `List.isSuffixOf` l

controlMasterArgs :: [String]
controlMasterArgs =
  [ "-o",
    "ControlMaster=auto",
    "-o",
    "ControlPath=/tmp/nammayatri-sftp-%C.sock",
    "-o",
    "ControlPersist=300"
  ]

runSftpListBatch ::
  (EncFlow m r, MonadIO m) =>
  SFTPConfig ->
  [String] ->
  m (Either Text [(String, [(Bool, String)])])
runSftpListBatch _ [] = pure $ Right []
runSftpListBatch cfg dirs = do
  let script =
        unlines $
          concatMap (\d -> ["-cd " ++ d, "-pwd", "-ls -l"]) dirs
            ++ ["quit"]
  logDebug $
    "runSftpListBatch: dirsCount=" <> show (length dirs)
      <> " dirs=" <> show dirs
  eOut <- runSftpBatch cfg script
  case eOut of
    Left err -> do
      logDebug $ "runSftpListBatch: sftp error: " <> err
      pure $ Left err
    Right out -> do
      let parsed = parseSftpListOutput out
      logDebug $
        "runSftpListBatch: parsedBlocks=" <> show (length parsed)
          <> " summary=" <> show [(p, length es) | (p, es) <- parsed]
      pure $ Right parsed

runSftpBatch ::
  (EncFlow m r, MonadIO m) =>
  SFTPConfig ->
  String ->
  m (Either Text String)
runSftpBatch cfg@SFTPConfig {..} script = do
  let portArg = P.show port
      userAtHost = T.unpack username ++ "@" ++ T.unpack host
  if isJust privateKey64 || isJust keyPath
    then do
      ePrep <- liftIO $ prepareSshIdentityFile cfg
      case ePrep of
        Left err -> pure $ Left err
        Right (keyFile, cleanup) ->
          liftIO $
            runSftpProcess
              "sftp"
              ( [ "-i",
                  keyFile,
                  "-P",
                  portArg,
                  "-b",
                  "-",
                  "-o",
                  "StrictHostKeyChecking=no"
                ]
                  ++ controlMasterArgs
                  ++ [userAtHost]
              )
              script
              `Exception.finally` cleanup
    else do
      mbPwd <- mapM decrypt password
      case mbPwd of
        Nothing -> pure $ Left "sftp listing: need password, keyPath, or privateKey64+keyPath"
        Just decrypted ->
          liftIO $
            runSftpProcess
              "sshpass"
              ( [ "-p",
                  T.unpack decrypted,
                  "sftp",
                  "-P",
                  portArg,
                  "-b",
                  "-",
                  "-o",
                  "StrictHostKeyChecking=no"
                ]
                  ++ controlMasterArgs
                  ++ [userAtHost]
              )
              script

runSftpProcess :: String -> [String] -> String -> IO (Either Text String)
runSftpProcess prog args script = do
  (exitCode, out, err) <- readProcessWithExitCode prog args script
  case exitCode of
    ExitSuccess -> pure $ Right out
    ExitFailure code ->
      pure $
        Left $
          T.pack prog <> " batch failed (exit " <> show code <> "): "
            <> T.pack err
            <> " | stdout: "
            <> T.pack out

parseSftpListOutput :: String -> [(String, [(Bool, String)])]
parseSftpListOutput out =
  let lns = lines out
      marker = "Remote working directory: "
      raw = collectBlocks lns marker
      seen0 = Set.empty
      dedup = dedupOnFirst seen0 raw
   in [(absPath, mapMaybe parseLsLine body) | (absPath, body) <- dedup]
  where
    collectBlocks [] _ = []
    collectBlocks (l : rest) m
      | m `List.isPrefixOf` l =
        let absPath = drop (length m) l
            (block, after) = break (m `List.isPrefixOf`) rest
         in (absPath, block) : collectBlocks after m
      | otherwise = collectBlocks rest m

    dedupOnFirst _ [] = []
    dedupOnFirst seen ((p, b) : rest)
      | p `Set.member` seen = dedupOnFirst seen rest
      | otherwise = (p, b) : dedupOnFirst (Set.insert p seen) rest

parseLsLine :: String -> Maybe (Bool, String)
parseLsLine raw =
  case dropWhile Char.isSpace raw of
    "" -> Nothing
    s@(c : _)
      | c == 'd' || c == '-' ->
        case reverse (words s) of
          (name : _)
            | not (null name),
              name /= ".",
              name /= ".." ->
              Just (c == 'd', name)
          _ -> Nothing
      | otherwise -> Nothing

unzipFirstCsv :: LBS.ByteString -> LBS.ByteString
unzipFirstCsv zipBytes =
  let archive = toArchive zipBytes
      entries = zEntries archive
      csvEntries = filter (\e -> ".csv" `List.isSuffixOf` map Char.toLower (eRelativePath e)) entries
   in case csvEntries of
        (e : _) -> fromEntry e
        [] -> case entries of
          (e : _) -> fromEntry e
          [] -> zipBytes -- fallback: return as-is

prepareSshIdentityFile :: SFTPConfig -> IO (Either Text (FilePath, IO ()))
prepareSshIdentityFile SFTPConfig {privateKey64, keyPath} =
  case (privateKey64, keyPath) of
    (Nothing, Nothing) -> pure $ Left "SFTP: no key identity (privateKey64 + keyPath, keyPath, or password)"
    (Nothing, Just kp) ->
      pure $ Right (T.unpack kp, pure ())
    (Just _, Nothing) ->
      pure $
        Left $
          "SFTP: privateKey64 requires keyPath (local file path to write the decoded PEM)"
    (Just b64, Just destTxt) ->
      case B64.decode (TE.encodeUtf8 b64) of
        Left err ->
          pure $ Left $ "privateKey64: invalid base64 (" <> T.pack err <> ")"
        Right keyBytes -> do
          let path = T.unpack destTxt
          createDirectoryIfMissing True (takeDirectory path)
          BS.writeFile path keyBytes
          setFileMode path 0o600
          pure $ Right (path, pure ())

normalizeRelPath :: Text -> Text
normalizeRelPath =
  T.intercalate "/"
    . filter (\s -> s /= "." && not (T.null s))
    . T.splitOn "/"

guardRemoteName :: Text -> Either Text Text
guardRemoteName raw0 =
  let raw = T.dropWhile (== '/') raw0
   in if
          | T.null raw -> Left "empty remote file name from SFTP listing"
          | "\\" `T.isInfixOf` raw -> Left $ "unsafe remote file name: " <> raw
          | otherwise ->
            let n = normalizeRelPath raw
             in if
                    | T.null n -> Left "empty remote file name from SFTP listing"
                    | any (\s -> s == "..") (T.splitOn "/" n) ->
                      Left $ "unsafe remote file name: " <> raw
                    | otherwise -> Right n

sliceCsvPayload :: Int -> Maybe Int -> LBS.ByteString -> Either Text (LBS.ByteString, Int)
sliceCsvPayload nextRowIndex mLimit bytes =
  let ls = map (BSL8.filter (/= '\r')) (BSL8.split '\n' bytes)
   in case ls of
        [] -> Left "empty CSV payload"
        (hdr : rest) ->
          let restLen = length rest
              start = min nextRowIndex restLen
              available = restLen - start
              takeCount = maybe available (\n -> min n (max 0 available)) mLimit
              chunk = take takeCount (drop start rest)
              out = LBS.intercalate (BSL8.singleton '\n') (hdr : chunk)
           in Right (out, takeCount)

downloadRemoteBytes :: (EncFlow m r, MonadIO m) => SFTPConfig -> Text -> m (Either Text LBS.ByteString)
downloadRemoteBytes cfg@(SFTPConfig {..}) remoteBaseName =
  let fname = T.unpack remoteBaseName
      remoteFile = case remotePath of
        Just rp
          | not (T.null rp) ->
            T.unpack (T.dropWhileEnd (== '/') rp) </> fname
        _ -> fname
   in do
        tmpDir <- liftIO getTemporaryDirectory
        let localPath = tmpDir </> fname
            portArg = P.show port
            userAtHost = T.unpack username <> "@" <> T.unpack host
        liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
        result <-
          if isJust privateKey64 || isJust keyPath
            then do
              ePrep <- liftIO $ prepareSshIdentityFile cfg
              case ePrep of
                Left err -> pure $ Left err
                Right (keyFile, cleanup) ->
                  liftIO $
                    fetchWithKeyIO keyFile userAtHost remoteFile localPath portArg
                      `Exception.finally` cleanup
            else fetchWithPassword cfg remoteFile localPath portArg
        case result of
          Left err -> pure $ Left err
          Right _ -> do
            contents <- liftIO $ LBS.readFile localPath
            liftIO $ removeFile localPath
            pure $ Right contents

fetchWithKeyIO ::
  FilePath ->
  String ->
  String ->
  String ->
  String ->
  IO (Either Text ())
fetchWithKeyIO keyPath userAtHost remoteFile localPath portArg = do
  let sftpCmd = "get " <> remoteFile <> " " <> localPath <> "\nquit\n"
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "sftp"
      ( [ "-i",
          keyPath,
          "-P",
          portArg,
          "-o",
          "StrictHostKeyChecking=no"
        ]
          ++ controlMasterArgs
          ++ [userAtHost]
      )
      sftpCmd
  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code ->
      pure $ Left $ "SFTP get failed (exit " <> show code <> "): " <> T.pack stderr

fetchWithPassword ::
  (EncFlow m r, MonadIO m) =>
  SFTPConfig ->
  String ->
  String ->
  String ->
  m (Either Text ())
fetchWithPassword cfg remoteFile localPath portArg = do
  mbPassword <- mapM decrypt cfg.password
  case mbPassword of
    Nothing -> pure $ Left "SFTP download: use password, or keyPath, or privateKey64 with keyPath"
    Just decryptedPassword -> do
      let scpTarget = T.unpack cfg.username <> "@" <> T.unpack cfg.host <> ":" <> remoteFile
      (exitCode, _stdout, stderr) <-
        liftIO $
          readProcessWithExitCode
            "sshpass"
            ( [ "-p",
                T.unpack decryptedPassword,
                "scp",
                "-P",
                portArg,
                "-o",
                "StrictHostKeyChecking=no"
              ]
                ++ controlMasterArgs
                ++ [scpTarget, localPath]
            )
            ""
      case exitCode of
        ExitSuccess -> pure $ Right ()
        ExitFailure code ->
          pure $ Left $ "SCP failed (exit " <> show code <> "): " <> T.pack stderr
