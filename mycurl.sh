#!/bin/bash  

curl -d '{
    "locationInventory" : 1
}' -X POST http://localhost:3000/locations-inventory
