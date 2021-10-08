we always prefer a single runDB so that if one fails the whole DB transaction will roll back. otherwise we get orphan rows, where one DB operation ran successfully, the other failed, now our data is out of sync...

---------------------------------------------------------------------------------

 where
    defaultLocationId = toSqlKey (1 :: Int64)

newtype Sum = Sum { unSum :: Int }

coerce 2 :: Sum
coerce (Sum 2) :: Int

requireCheckJsonBody :: forall a m. (MonadHandler m, FromJSON a) => m ()


---------------------------------------------------------------------------------

:: type annotation
@ type application


---------------------------------------------------------------------------------

example found inside of postWharehouseAquiresBookR 

 liftIO $ print apiBook
myTodo bring the below line back from comment
 _ <- runDB $ insertProductBy (toBook apiBook) theTime whareHouseId

 contrasted with the longer 

 _ <- runDB $ do
     prodId <- insert $ Product defaultLocationId theTime 
let theNewBook = Book aNewProduct  (partialBookWithLocationAuthor partialbook) (partialBookWithLocationGenre partialbook) (partialBookWithLocationPageCount partialbook) (partialBookWithLocationCost partialbook)
     insert $ toBook apiBook prodId

---------------------------------------------------------------------------------


-- ok so postWharehouseAquiresProductR is interesting but not practical, how would you pass in toProduct?

inferred type
postWharehouseAquiresProductR
  :: (PersistEntity a, FromJSON t,
      PersistEntityBackend a ~ SqlBackend) =>
     (t -> ProductId -> a) -> HandlerFor App b
postWharehouseAquiresProductR toProduct = do 
    gotYesod <- getYesod 
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod    
    theTime <- getCurrentTime
    apiThing <- requireCheckJsonBody
    theInsertion <- runDB $ insertProduct (toProduct apiThing) theTime whareHouseId
    print theInsertion
    sendResponseStatus status201 ("PRODUCT stocked in store" :: Text)


---------------------------------------------------------------------------------


-- deleteAllBooks :: Handler ()

-- QQQ 1
--so without the above type, it is ambigious and get this error... is there a way to constrain the type to only a SqlBackend since I'm only using PostgreSQL

--    • Couldn't match type ‘BaseBackend backend0’ with ‘SqlBackend’
--         arising from a use of ‘deleteWhere’
--       The type variable ‘backend0’ is ambiguous
--     • In the expression: deleteWhere ([] :: [Filter Book])
--       In an equation for ‘deleteAllBooks’:
--           deleteAllBooks = deleteWhere ([] :: [Filter Book])
--     • Relevant bindings include
--         deleteAllBooks :: ReaderT backend0 m0 ()
--           (bound at src/Handler/Products.hs:165:1)

-- runDB
--   :: YesodPersist site =>
--      ReaderT (YesodPersistBackend site) (HandlerFor site) a
--      -> HandlerFor site a

-- :t deleteWhere
-- (MonadIO m, PersistQueryWrite backend, PersistEntity record,
--       PersistEntityBackend record ~ BaseBackend backend) =>
--      [Filter record] -> ReaderT backend m ()

-- deleteWhere ([] :: [Filter Book])
--   :: (MonadIO m, PersistQueryWrite backend,
--       BaseBackend backend ~ SqlBackend) =>
--    ReaderT backend m ()
-- type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerFor site)

-- runDB
-- YesodDB site a -> HandlerFor site a
-- HandleFor
-- HandlerFor { unHandlerFor :: HandlerData site site -> IO a }
-- ReaderT (YesodPersistBackend site) (HandlerFor site) a -> HandlerData site site -> IO a

-- the a is really only the thing that we have control over 
-- how could we fill in the other type params? Type applications on runDB 

-- Handler is a type alias that is generated 

---- ReaderT (YesodPersistBackend site) (HandlerFor site) a -> HandlerData site site -> IO a


--deleteAllBooks :: HandlerFor site () -- this compiles because this is just an alias for Handler ()
-- deleteAllBooks = runDB @App @() $ deleteWhere ([] :: [Filter Book]) 
-- ~
-- deleteAllBooks = (runDB :: YesodDB App () -> HandlerFor App ()) $ deleteWhere ([] :: [Filter Book])

-- deleteWhere, a function like this is only operating on the polymorphic a, but it is constrained backend 
deleteAllBooks :: Handler ()
deleteAllBooks = runDB $ deleteWhere ([] :: [Filter Book])

---------------------------------------------------------------------------------

-- QQQ 2 how can we pass this product in as an argument, the types get tricky  

-- deleteTypeOfProduct :: (MonadIO m, PersistQueryWrite backend,
--         BaseBackend backend ~ SqlBackend) =>
--         record -> ReaderT backend m ()

-- deleteTypeOfProduct :: Handler ()
--  deleteTypeOfProduct :: forall site record p.
--                                           (PersistQueryWrite (YesodPersistBackend site),
--                                            YesodPersist site, PersistEntity record,
--                                            PersistEntityBackend record
--                                            ~ BaseBackend (YesodPersistBackend site)) =>
--      
-- the Proxy a only exists at type level, not at value level                                     p -> Handler ()
-- data Proxy a = Proxy

-- AAA 
deleteTypeOfProduct deleteTypeOfProduct'' are the answers to the above question

---------------------------------------------------------------------------------
