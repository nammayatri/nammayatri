{-# LANGUAGE QuasiQuotes #-}

module Main where

import Alchemist.App
import Kernel.Prelude
import System.Directory (createDirectoryIfMissing)

storageYamlFilePath :: FilePath
storageYamlFilePath = "./tests/storage.yaml"

apiYamlFilePath :: FilePath
apiYamlFilePath = "./tests/api.yaml"

generateAllExample :: IO ()
generateAllExample = do
  mapM_ (createDirectoryIfMissing True) ["./output/Beam", "./output/Queries", "./output/Domain/Type", "./output/SQL"]
  mkBeamTable "./output/Beam" storageYamlFilePath
  mkBeamQueries "./output/Queries" storageYamlFilePath
  mkDomainType "./output/Domain/Type" storageYamlFilePath
  mkSQLFile "./output/SQL" storageYamlFilePath
  mkServantAPI "./output" apiYamlFilePath
  mkDomainHandler "./output/Domain" apiYamlFilePath

main :: IO ()
main = pure ()
