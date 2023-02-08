module Main where

import App (runService)
import Kernel.Prelude

main :: IO ()
main = runService identity
