module Main where

import "public-transport-search-consumer" App (runPublicTransportSearchConsumer)
import Beckn.Prelude

main :: IO ()
main = runPublicTransportSearchConsumer identity
