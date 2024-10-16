module Main (main) where

import           RouteRequest (routeRequest)
import           SocketIO     (runServer)

-- | The main entry point for the server. It starts the server and routes incoming
--   HTTP requests using the `routeRequest` function.
main :: IO ()
main = runServer routeRequest
