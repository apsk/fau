class MArray tmp e m => TMA tmp m res i e | tmp -> m res where
  temp :: (i, i) -> e -> m (tmp i e)
  done :: tmp i e -> m (res i e)

instance Ix i => TMA IOArray IO Array i e where
  temp = newArray
  done = unsafeFreeze

instance (Ix i, IArray UArray e, MArray IOUArray e IO) =>
    TMA IOUArray IO UArray i e
  where
    temp = newArray
    done = unsafeFreeze
