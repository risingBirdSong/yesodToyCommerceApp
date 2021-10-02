module DB.StoreInventory where

import Import hiding ((==.), on)

import Database.Esqueleto.Experimental


getStoreInventory :: StockLocationId -> DB [Entity Food]
getStoreInventory locationId = do
    select $ do
        (foods :& prods) <- 
            from $ Table @Food 
            `InnerJoin` Table @Product 
            `on` (\(food :& prods) -> 
                    food ^. FoodProductId ==. prods ^. ProductId)
        where_ (prods ^. ProductStockLocationId ==. val locationId)   
        pure foods
            
            
