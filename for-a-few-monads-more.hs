
main = do
  print "Hello"
  print $ join' (Just (Just "Yo"))
  print $ join' (Just (Just (Just "Yo")))


-- join' :: (Monad m) => m (m a) -> m a
-- -- join' mx = mx >>= id
-- join' mx = mx >>= (\x -> )
