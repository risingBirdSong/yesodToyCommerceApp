{-# LANGUAGE NoImplicitPrelude #-}
module Import.Lifted
    ( module Import
    , module Import.Lifted
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import hiding (getCurrentTime, print)
import qualified Import.NoFoundation as Default

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Default.getCurrentTime

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Default.print