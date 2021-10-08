module DB.TransferProductsLocation where

-- myTodo 
-- import DB.ImportEsqueleto
-- import Database.Esqueleto.Experimental 

-- findProduct prodId = do 
--     select $ do
--         (prod :& food) <- 
--             from $ Table @Product 
--                 `leftJoin` Table @Food
--                 `on` (\(prod :& food) -> 
--                         prod ^. ProductId ==. food ?. FoodProductId
--                     )
--         pure (prod ^. ProductId , food ?. FoodProductId)