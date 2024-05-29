module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai
import qualified Api (app1)

main :: IO ()
main = run 8081 app1

