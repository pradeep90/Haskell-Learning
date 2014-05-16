
-- `on` is AWESOME!
sortBy (compare `on` negate . length) [[1..6], [1..2], [1..4]]
