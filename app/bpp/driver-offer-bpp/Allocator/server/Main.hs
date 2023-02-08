module Main where

import App
import Kernel.Prelude

main :: IO ()
main = runDriverOfferAllocator identity
