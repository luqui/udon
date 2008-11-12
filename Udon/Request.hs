module Udon.Request 
    ( Request, request
    , runRequest, runRequestHandler
    )
where

import Control.Applicative
import Control.Monad
import Control.Arrow hiding (pure)
import Data.Monoid

newtype ReqList i o a = ReqList [(o, i -> a)]

instance Functor (ReqList i o) where
    fmap f (ReqList xs) = ReqList ((fmap.second.fmap) f xs)

instance Monoid (ReqList i o a) where
    mempty = ReqList []
    mappend (ReqList a) (ReqList b) = ReqList (a ++ b)



data Request i o a
    = Return a
    | Request (ReqList i o (Request i o a))

instance Functor (Request i o) where
    fmap f (Return x) = Return (f x)
    fmap f (Request cs) = Request (fmap (fmap f) cs)

instance Applicative (Request i o) where
    pure = Return

    Return f <*> Return x = Return (f x)
    Return f <*> Request csx = Request (fmap (fmap f) csx)
    Request csf <*> Return x = Request (fmap (fapp x) csf)
    Request csf <*> Request csx = Request (freqs `mappend` xreqs)
        where
        freqs = fmap (<*> Request csx) csf
        xreqs = fmap (Request csf <*>) csx

instance Monad (Request i o) where
    return = Return
    Return x >>= f = f x
    Request cs >>= f = Request (fmap (>>= f) cs)

fapp x ff = ff <*> pure x

request :: o -> Request i o i
request o = Request (ReqList [(o, Return)])


runRequest :: Request i o a -> Either [(o, i -> Request i o a)] a
runRequest (Return x)            = Right x
runRequest (Request (ReqList l)) = Left l

runRequestHandler :: (Monad m) => (o -> m i) -> Request i o a -> m a
runRequestHandler handler req = 
    case runRequest req of
        Right x -> return x
        Left [] -> fail "No more requests to handle"
        Left ((o,f):_) -> runRequestHandler handler . f =<< handler o
