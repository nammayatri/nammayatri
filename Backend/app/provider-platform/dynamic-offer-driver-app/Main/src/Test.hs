{-# LANGUAGE BangPatterns #-}

module Test where

-- import Data.Aeson as A
-- import Data.Aeson.Types (parseEither, Parser, (.:))
-- import Data.Text (pack, unpack)
-- import Data.Aeson.Key (fromText)
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text.Encoding as TE
-- import Data.Time.Clock hiding (getCurrentTime)
-- import Data.Fixed

-- import Domain.Types.Merchant.TransporterConfig
-- import GHC.Generics
import qualified Data.Text as Text
import Kernel.Prelude as KP

-- import Kernel.Types.Time

-- import Control.Monad (mzero)
-- extractNameFromValue :: Value -> Either String String
-- extractNameFromValue (Object obj) =
--     case HM.lookup "name" obj of
--         Just (String name) -> Right (unpack name)
--         _ -> Left "Name field not found or is not a string"
-- extractNameFromValue _ = Left "Input is not a JSON object"

readWithInfo :: (Read a, Show a) => String -> a
readWithInfo s = case KP.readMaybe s of
  Just val -> val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s

readWithInfo' :: (Read a, Show a) => String -> Maybe a
readWithInfo' s = case KP.readMaybe s of
  Just val -> Just val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s

main :: IO ()
main = do
  -- _ <- test
  -- let x = "String \"http://localhost:8016/ui/<DOMAIN>/media?filePath=<FILE_PATH>	\""
  -- let y = (  x) :: (Maybe AadhaarImageResizeConfig)
  putStrLn $ ("result: " :: String)
