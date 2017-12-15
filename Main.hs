{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Check to see if prometheus-effect-1.1.0 correctly counts
all 'incCounter' calls under high load

-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Network.HTTP.Types.Status
import Prometheus
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

data Metrics = Metrics
  { iterations :: Counter
  }

main :: IO ()
main = do
  -- Check we're running on multiple threads
  numThreads <- getNumCapabilities
  putStrLn $ "Running on " ++ (show numThreads) ++ " threads"

  -- Set up the registry for Prometheus
  (metrics, registry) <- buildRegistry $ do
    iterations <- register "iterations" "Total completed iterations" mempty counter
    return Metrics{..}

  -- Wire up /metrics endpoint
  _ <- forkIO $ Warp.run 2000 $ publishRegistryMiddleware ["metrics"] registry $ \_ mkRes ->
    mkRes (Wai.responseLBS notFound404 mempty mempty)

  -- Increment 1,000,000 times using all threads
  putStrLn "Incrementing... Please wait."
  replicateConcurrently_ 1000000 (incCounter (iterations metrics))

  -- Wait until you can check the count
  putStrLn "Done counting. Check http://localhost:2000/metrics for count."
  _ <- getLine

  return ()

