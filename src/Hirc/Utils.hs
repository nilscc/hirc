module Hirc.Utils
    ( delayUntil
    ) where

import Control.Monad.Trans ( MonadIO(..) )
import Data.Time ( UTCTime, diffUTCTime, getCurrentTime )
import Control.Concurrent ( threadDelay )
import Control.Monad (when)

-- | Delay a thread until a given `UTCTime`
delayUntil :: MonadIO m => UTCTime -> m ()
delayUntil time = do
  now <- liftIO getCurrentTime
  let tdiff = diffUTCTime time now
  when (tdiff > 0) $ do
    -- delay for max. 5 minutes before checking again
    liftIO . threadDelay $ 1000000 * (round tdiff `mod` (5*60))
    delayUntil time