module Main where

import Allocator.App
import Prelude

main :: IO ()
main = runAllocator id
