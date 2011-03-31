module Utils
    ( delayUntil
    ) where

import Control.Monad.Trans
import Data.Time
import Control.Concurrent

-- | Delay a thread until a given `UTCTime`
delayUntil :: MonadIO m => UTCTime -> m ()
delayUntil time = do
  now <- liftIO getCurrentTime
  let tdiff = diffUTCTime time now
  if tdiff > 0 then do
    -- delay for max. 5 minutes before checking again
    liftIO . threadDelay $ 1000000 * (round tdiff `mod` (5*60))
    delayUntil time
   else
    return ()
