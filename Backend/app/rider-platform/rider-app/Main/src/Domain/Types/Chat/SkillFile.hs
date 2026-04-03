{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Chat.SkillFile
  ( SkillFile (..),
    SkillExample (..),
    PermittedTool (..),
    allPermittedTools,
    isToolPermitted,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)
import Kernel.Prelude

-- | Represents a permitted tool that can be used by the LLM
-- These are the only tools allowed for skill-based tool calling
data PermittedTool
  = Search
  | Select
  | Confirm
  | Cancel
  | GetRideStatus
  | GetProfile
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

instance FromJSON PermittedTool where
  parseJSON = withText "PermittedTool" $ \case
    "search" -> pure Search
    "select" -> pure Select
    "confirm" -> pure Confirm
    "cancel" -> pure Cancel
    "getRideStatus" -> pure GetRideStatus
    "getProfile" -> pure GetProfile
    other -> fail $ "Unknown permitted tool: " ++ show other

instance ToJSON PermittedTool where
  toJSON = \case
    Search -> String "search"
    Select -> String "select"
    Confirm -> String "confirm"
    Cancel -> String "cancel"
    GetRideStatus -> String "getRideStatus"
    GetProfile -> String "getProfile"

-- | All permitted tools as a list
allPermittedTools :: [PermittedTool]
allPermittedTools = [minBound .. maxBound]

-- | Check if a tool name is in the permitted list
isToolPermitted :: Text -> Bool
isToolPermitted toolName = toolName `elem` map (toLower . show) allPermittedTools

-- | An example conversation for the skill
data SkillExample = SkillExample
  { userMessage :: !Text,
    assistantResponse :: !Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON SkillExample where
  parseJSON = withObject "SkillExample" $ \v ->
    SkillExample
      <$> v .: "userMessage"
      <*> v .: "assistantResponse"

instance ToJSON SkillExample where
  toJSON SkillExample {..} =
    object
      [ "userMessage" .= userMessage,
        "assistantResponse" .= assistantResponse
      ]

-- | Skill file configuration loaded from YAML
-- Defines the behavior and capabilities of a chat skill
data SkillFile = SkillFile
  { name :: !Text,
    description :: !Text,
    allowedTools :: ![PermittedTool],
    systemPrompt :: !Text,
    examples :: ![SkillExample]
  }
  deriving (Generic, Show, Eq)

instance FromJSON SkillFile where
  parseJSON = withObject "SkillFile" $ \v ->
    SkillFile
      <$> v .: "name"
      <*> v .: "description"
      <*> v .: "allowedTools"
      <*> v .: "systemPrompt"
      <*> v .:? "examples" .!= []

instance ToJSON SkillFile where
  toJSON SkillFile {..} =
    object
      [ "name" .= name,
        "description" .= description,
        "allowedTools" .= allowedTools,
        "systemPrompt" .= systemPrompt,
        "examples" .= examples
      ]
