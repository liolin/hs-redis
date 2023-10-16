module Main (main) where

import Lib (runTCPServer, echo)

main :: IO ()
main = runTCPServer Nothing "3000" echo

