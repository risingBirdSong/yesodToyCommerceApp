from src/Handler/Products.hs

-- so this is using query params but maybe better to put this info in an apiJson?
-- postWhareHouseAcquiresGenericProductR :: Handler Value 
-- postWhareHouseAcquiresGenericProductR = do 
--     gotYesod <- getYesod
--     let whareHouseId = appWharehouseLocation . appSettings $ gotYesod 
--     theTime <- getCurrentTime
--     --example of query parameters
--     getParameters <- reqGetParams <$> getRequest -- Params: [("product","book")]
--     let mybProduct = lookup "product" getParameters -- Just "book"
--     case mybProduct of
--         Nothing -> sendResponseStatus status201 ("BOOK stocked in store" :: Text)
--         Just "book" -> do
--             apiBook :: CreateBook <- requireCheckJsonBody  
--             _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
--             sendResponseStatus status201 ("book inserted into wharehouse" :: Text)

--         Just "food" -> do
--             apiFood :: CreateFood <- requireCheckJsonBody
--             _ <- runDB $ insertProduct (toFood apiFood) theTime whareHouseId
--             sendResponseStatus status201 ("food inserted into wharehouse" :: Text)


alternative ways to do this instead of type application using Proxies

-- deleteTypeOfProduct :: forall a m. (PersistEntityBackend a ~ SqlBackend, MonadIO m, PersistEntity a)=> Proxy a -> SqlPersistT m ()
-- deleteTypeOfProduct _ = deleteWhere ([] :: [Filter a])

-- deleteTypeOfProduct' (Proxy :: Proxy Book)
-- deleteTypeOfProduct' (Proxy @Book)    




-- postTransferAProdFromLocAtoB_MaybeR :: Handler Value
-- postTransferAProdFromLocAtoB_MaybeR = do
--     TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
--     mSuccess :: Either [String] a  <- runDB $ runValidateT $ do 
--         disputeNothing "prod was not found" $ productAtLocation productId transferOrigin
--         m a >>= (a -> Either l b)

        -- eStatus <- runValidateT $ do my validations
        -- case eStatus of
        --   Left err -> Left err
        --   Right _ -> Right <$> ... other sql stuff

    -- f :: IO String
    -- f = do
    --   putStrLn "Hey" :: IO ()
    --   pure "My String" :: IO String -- m a >>= (a -> m b) -- (>>) -- m a >>= (\() -> m b)

    -- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
    -- newtype ValidateT e m a = ValidateT { getValidateT :: forall s. StateT (MonoMaybe s e) (ExceptT e m) a }
    -- nameOfOrigin <- ValidateT $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []


    --     ensureDestinationExists <- MaybeT $ selectFirst [StockLocationId ==. (toSqlKey transferDestination)] []
        -- nameOfOrigin <- pure $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []
        -- nameOfOrigin <- $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []
        -- nameOfOrigin <- refuteNothing $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []

    --     nameOfDestination <- MaybeT $ selectFirst [StockLocationId ==. (toSqlKey transferDestination)] []
    --     pure (ensureProdLoc, ensureDestinationExists, nameOfOrigin, nameOfDestination)
    -- case mSuccess of
    --     Nothing -> sendResponseStatus status400 ("something went wrong" :: Text)
    --     Just (_,_,origin, destination) ->  pure $ object ["status" .= ("success" :: Text), "origin" .= origin, "destination" .= destination]

-- disputeNothing :: (MonadValidate e m) => e -> Maybe a -> m ()
-- disputeNothing err mVal = case mVal of
--     Nothing -> dispute err -- refute :: e -> m a
--     Just _ -> pure ()

-- refuteNothing :: (MonadValidate e m) => e -> Maybe a -> m a
-- refuteNothing err = fromMaybe (refute err)


-- (<*>) :: m (a -> b) -> m a -> m b
-- (<*>) mf ma = do
    -- eval mf
    -- eval ma
    -- refute 1 *> refute 2
    -- f a else Left errs

-- disputeNothing returns a ValidateT
-- disputeNothing take in an error message and a maybe, and case on the maybe
-- Nothing is refuted
-- Just is pured 



 
    -- MaybeT 
    -- fusion in Haskell works because of purity, but in a runDB with sql were working with effects and therefore fusion is much more difficult not necc. desired
    -- just put the case of inside the runDB not outside 
    -- void $ runDB $ updateWhere [ProductId ==. (toSqlKey productId)] [ProductStockLocationId =. (toSqlKey transferLocation)]




-- maybeToValidationA = 

-- ensureProdAtLocationAndDestination :: 
-- ensureProdAtLocationAndDestination productId transferOrigin = 
--     case productAtLocation productId transferOrigin of
--         Nothing -> _Failure # (pack "prod was not at location")
--         Just val -> _Success # (val)




-- data TransferProdLocationFromAToBJson = TransferProdLocationFromAToBJson {
-- -- so both these needs to be used with toSqKey 
--         productId :: Key Product ,
--         transferOrigin :: Key LocationId , 
--         transferDestination :: Key LocationId
--     }
--  we might be able to derive stock for TransferProdLocationFromAToBJson with these more specific keys, but if not...
-- instance FromJSON TransferProdLocationFromAToBJson where
--   parseJSON = withObject "TransferProdLocationFromAToBJson" $ \v -> TransferProdLocationFromAToBJson
--      <$> (toSqlKey <$> o .: "productId")
--      <*> (toSqlKey <$> o .: "transferOrigin")
-- ...


validationExampeA = maybeToValidation (pack "error a") (Just 1) <*
                    maybeToValidation (pack "error b") (Just 2) <*
                    maybeToValidation (pack "error c") (Just 3) <*
                    maybeToValidation (pack "error d") (Just 4) 
                    -- Success 1


validationExampeB = maybeToValidation (pack " error a ") (Nothing) <*
                    maybeToValidation (pack " error b ") (Nothing) <*
                    maybeToValidation (pack " error c ") (Just 3)  <*
                    maybeToValidation (pack " error d ") (Nothing) 
                    -- Failure " error a  error b error d "


postTransferAProdFromLocAtoB_MaybeTR :: Handler Value
postTransferAProdFromLocAtoB_MaybeTR = do
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    mSuccess  <- runDB $ runMaybeT $ do 
        ensureProdLoc <- MaybeT $ productAtLocation productId transferOrigin
        ensureDestinationExists <- MaybeT $ selectFirst [StockLocationId ==. transferDestination] []
        -- pure updated
        pure (ensureProdLoc, ensureDestinationExists)
    case mSuccess of
        Nothing -> sendResponseStatus status400 ("check validation" :: Text)
        -- Just (ensureProdLoc, ensureDestinationExists , origin, destination) -> do
        Just _ -> do
            myb <- runDB $ runMaybeT $ do
                mybNameOfOrigin <- MaybeT $ selectFirst [StockLocationId ==. transferOrigin] []
                mybNameOfDestination <- MaybeT $ selectFirst [StockLocationId ==. transferDestination] []
                pure (mybNameOfOrigin, mybNameOfDestination)
            case myb of
                Just ((Entity _ nameOfOrigin), (Entity _ nameOfDestination)) -> runDB $ do 
                    updated <- updateWhere [ProductId ==. productId] [ProductStockLocationId =. transferDestination]
                    sendResponseStatus status201 ("transfered" :: Text)
                _ -> sendResponseStatus status400 ("something went wrong" :: Text)
