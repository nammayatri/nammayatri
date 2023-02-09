module Main where

import App (runSearchResultAggregator)
import Kernel.Prelude

main :: IO ()
main = runSearchResultAggregator identity
