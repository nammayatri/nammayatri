module Main where

import App (runSearchResultAggregator)
import Beckn.Prelude

main :: IO ()
main = runSearchResultAggregator identity
