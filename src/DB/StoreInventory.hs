module DB.StoreInventory where

import Import hiding ((==.), on)

import Database.Esqueleto.Experimental


            
-- just getting prod
likeSelectList_Esq :: 
    Key StockLocation ->
    DB ([Entity Product])            
likeSelectList_Esq loc = select $ do
    prods <- from $ Table @Product
    where_ ((prods ^. ProductStockLocationId) ==. (val loc))
    pure prods 

-- getProductAndNameOfLoc prodId locA = 
--     select $ do
--         prod <- from $ table @Product
--         where_ ()  