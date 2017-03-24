module Util where

fromMaybeM :: Monad m => String -> Maybe a -> m a
fromMaybeM st = maybe (fail st) return
