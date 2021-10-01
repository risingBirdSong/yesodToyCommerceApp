{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Handler.Books where

import Import 

data ABook = ABook {aBook :: Entity Book}
    deriving (Generic)

instance FromJSON ABook
instance ToJSON ABook

getStoreAquiresBookR :: Handler ()
getStoreAquiresBookR = do 
    liftIO $ print "hitting" 
    pure ()

postStoreAquiresBookR :: Handler Value 
postStoreAquiresBookR = do
    theTime <- liftIO $ getCurrentTime
    book :: Book <- requireCheckJsonBody 
    liftIO $ print $ bookAuthor book 
    aNewProduct <- runDB $ insert $ Product theTime 
    let theNewBook = Book aNewProduct (bookAuthor book) -- (bookGenre book) (bookPageCount book) (bookCost book)
    insertedbook <- runDB $ insert $ theNewBook 
    -- liftIO $ print "book inserted"
    sendResponseStatus status201 ("Store acquired new book" ++ (show $ bookAuthor book))
    -- pure ()

--  productId ProductId
--     author Text
--     genre Text 
--     pageCount Int
--     cost Int
--     deriving Eq
--     deriving Show