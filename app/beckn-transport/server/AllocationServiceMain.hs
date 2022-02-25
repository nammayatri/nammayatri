module Main where

import App.Allocator
import Prelude

main :: IO ()
main = runAllocator id
