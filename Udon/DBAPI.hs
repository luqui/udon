
-- API for writing Database implementations
module Udon.DBAPI 
    ( Blob, Hash, showHash, Database(..), markAlive
    )
where

import Udon.Hash
import Udon.Database
