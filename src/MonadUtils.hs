module MonadUtils (module MonadUtils) where

someWhile :: (Monad f) => (a -> Bool) -> f a -> f [a]
someWhile predicate p = go []
  where
    go acc = do
      x <- p
      if predicate x
        then go (acc ++ [x])
        else pure acc
