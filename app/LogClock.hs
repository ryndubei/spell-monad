{-# LANGUAGE BlockArguments #-}
module LogClock
  ( LogClockH
  , logClock
  , logClockDebug
  , LogClock(..)
  , LogMessage
  , module Control.Monad.Log
  ) where

import Data.Text (Text)
import Control.Monad.Log
import FRP.Rhine
import Control.Monad.Trans.Class

type LogMessage msg = WithTimestamp (WithSeverity (Text, msg))

-- | Clock that logs each tick of the contained clock
data LogClock cl = LogClock Severity Text cl

instance (Monad m, Clock m cl, Time cl ~ UTCTime, tag ~ Tag cl, msg ~ LogMessage tag) => Clock (LoggingT msg m) (LogClock cl) where
  type Tag (LogClock cl) = Tag cl
  type Time (LogClock cl) = Time cl
  initClock (LogClock sev name cl) = do
    (rc, t0) <- lift $ initClock cl
    let rc' = hoistS lift rc >>> arrM \(t, tag) -> do
          logMessage (WithTimestamp (WithSeverity sev (name, tag)) t)
          pure (t, tag)
    pure (rc', t0)

instance GetClockProxy cl => GetClockProxy (LogClock cl)

type LogClockH m cl = HoistClock (LoggingT (LogMessage (Tag cl)) m) m (LogClock cl)

-- | 'LogClock' hoisted back into the original monad
--
-- NOTE: often the monad 'm' needs to be annotated explicitly when used with Rhine
logClock :: forall m msg cl. MonadLog (LogMessage msg) m => Severity -> Text -> cl -> (Tag cl -> msg) -> LogClockH m cl
logClock sev name cl f = HoistClock (LogClock sev name cl) (`runLoggingT` (logMessage . fmap (fmap (second f))))

logClockDebug :: forall m msg cl. MonadLog (LogMessage msg) m => Text -> cl -> (Tag cl -> msg) -> LogClockH m cl
logClockDebug = logClock Debug