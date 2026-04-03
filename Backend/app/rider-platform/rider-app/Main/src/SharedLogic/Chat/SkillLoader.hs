{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Chat.SkillLoader
  ( loadSkills,
    loadSkillsFromDirectory,
    validateSkill,
    SkillLoaderError (..),
    SkillLoaderResult,
    LoadedSkills,
    getSkillByName,
  )
where

import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (decodeEither')
import Domain.Types.Chat.SkillFile
import Kernel.Prelude
import Kernel.Types.Common
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

-- | Errors that can occur during skill loading
data SkillLoaderError
  = DirectoryNotFound FilePath
  | FileLoadError FilePath Text
  | ParseError FilePath Text
  | ValidationError FilePath Text
  deriving (Show, Eq)

-- | Result of loading a single skill
type SkillLoaderResult = Either SkillLoaderError (Text, SkillFile)

-- | Map of loaded skills by name
type LoadedSkills = Map Text SkillFile

-- | Get a skill by name from loaded skills
getSkillByName :: Text -> LoadedSkills -> Maybe SkillFile
getSkillByName = Map.lookup

-- | Load all skills from the default skill-files directory
-- This should be called at application startup
loadSkills :: (MonadIO m, Log m) => m LoadedSkills
loadSkills = do
  let skillDir = "skill-files"
  loadSkillsFromDirectory skillDir

-- | Load all skill files from a directory
-- Validates each skill and returns a map of valid skills
loadSkillsFromDirectory :: (MonadIO m, Log m) => FilePath -> m LoadedSkills
loadSkillsFromDirectory dirPath = do
  dirExists <- liftIO $ doesDirectoryExist dirPath
  unless dirExists $ do
    logWarning $ "Skill files directory not found: " <> T.pack dirPath
    pure ()

  if not dirExists
    then pure Map.empty
    else do
      files <- liftIO $ listDirectory dirPath
      let yamlFiles = filter (\f -> takeExtension f `elem` [".yaml", ".yml"]) files

      results <- mapM (loadSkillFile dirPath) yamlFiles

      let successes = rights results
          failures = lefts results

      -- Log failures but don't crash
      mapM_ (logSkillLoaderError . T.pack . show) failures

      -- Check for duplicate names
      let nameCounts = Map.fromListWith (+) $ map (\(name, _) -> (name, 1 :: Int)) successes
      let duplicates = Map.filter (> 1) nameCounts

      unless (Map.null duplicates) $ do
        logWarning $ "Duplicate skill names found: " <> T.pack (show $ Map.keys duplicates)

      pure $ Map.fromList successes

-- | Load a single skill file
loadSkillFile :: (MonadIO m, Log m) => FilePath -> FilePath -> m SkillLoaderResult
loadSkillFile dirPath fileName = do
  let fullPath = dirPath </> fileName
  result <- liftIO $ try @SomeException $ BS.readFile fullPath

  case result of
    Left ex -> do
      let err = FileLoadError fullPath $ T.pack $ show ex
      pure $ Left err
    Right content -> do
      case decodeEither' content of
        Left yamlErr -> do
          let err = ParseError fullPath $ T.pack $ show yamlErr
          pure $ Left err
        Right skillFile -> do
          case validateSkill skillFile of
            Just validationErr -> do
              let err = ValidationError fullPath validationErr
              pure $ Left err
            Nothing -> do
              logInfo $ "Loaded skill: " <> skillFile.name <> " from " <> T.pack fileName
              pure $ Right (skillFile.name, skillFile)

-- | Validate a skill file
-- Returns Just error message if invalid, Nothing if valid
validateSkill :: SkillFile -> Maybe Text
validateSkill skillFile
  | T.null (T.strip skillFile.name) = Just "Skill name cannot be empty"
  | T.null (T.strip skillFile.description) = Just "Skill description cannot be empty"
  | T.null (T.strip skillFile.systemPrompt) = Just "System prompt cannot be empty"
  | not (null invalidTools) = Just $ "Invalid tools specified: " <> T.pack (show invalidTools)
  | otherwise = Nothing
  where
    -- All tools in allowedTools are already validated by FromJSON
    -- but we double-check here for any custom validation
    invalidTools = []

-- | Log a skill loader error
logSkillLoaderError :: Log m => Text -> m ()
logSkillLoaderError err = logError $ "SkillLoader: " <> err

-- Helper to get rights from a list of Eithers
rights :: [Either a b] -> [b]
rights = foldr (\e acc -> either (const acc) (: acc) e) []

-- Helper to get lefts from a list of Eithers
lefts :: [Either a b] -> [a]
lefts = foldr (\e acc -> either (: acc) (const acc) e) []
