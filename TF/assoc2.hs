type Assoc a b = a -> Maybe b

emptyAssoc :: Assoc a b
emptyAssoc x = Nothing

lookupAssoc :: (Eq a) => a -> Assoc a b -> Maybe b
lookupAssoc x h = h x

updateAssoc ::
     (Eq a) => a -> b -> Assoc a b -> Assoc a b
updateAssoc k v h =
  \x -> if k == x then Just v
        else lookupAssoc x h

