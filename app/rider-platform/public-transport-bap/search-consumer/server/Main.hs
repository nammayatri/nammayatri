module Main where

import "public-transport-search-consumer" App (runPublicTransportSearchConsumer)
import Kernel.Prelude

main :: IO ()
main = runPublicTransportSearchConsumer identity
