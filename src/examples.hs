data BaldingError
  = RecedingFront
  | WhatHair
  deriving ( Show )

type Age = Int

data BrokeError
  = LivingAtHome Age
  | LivingUnderBridge
  deriving ( Show)

data MyError
  = MkBaldingError BaldingError
  | MkBrokeError BrokeError
  deriving ( Show )

CL.makeClassyPrisms ''BaldingError
CL.makeClassyPrisms ''BrokeError
CL.makeClassyPrisms ''MyError


derp :: (MonadError err m, AsMyError err) => Bool -> m String
derp b
  | b = pure "Hello World"
  | otherwise = throwError $ _MkBrokeError . _LivingAtHome CL.# 55

rawr :: (Monad m, AsMyError err) => m (Either err String)
rawr = runExceptT (derp False)