{-@ LIQUID "--no-termination" @-}

{-@ type AtLeastQuanta = {v: Double | 0.9 < v}  @-}

{-@
data VolDroplet = VolDroplet
  { volume :: AtLeastQuanta }
@-}
data VolDroplet = VolDroplet
  { volume :: Double }

{-@ mkDroplet :: v: AtLeastQuanta -> IO VolDroplet @-}
mkDroplet :: Double -> IO VolDroplet
mkDroplet v = return VolDroplet {volume = v}

mix :: VolDroplet -> VolDroplet -> IO VolDroplet
mix self other = return $ VolDroplet
  { volume = result_volume }
  where
    result_volume = volume self + volume other

-- this should incur some error
split :: VolDroplet -> IO (VolDroplet, VolDroplet)
split self = return (d1, d2)
  where
      d1 = self { volume = (volume self) / 2 }
      d2 = self { volume = (volume self) / 2 }

main = do
  -- after this, we should have incurred error due to the imprecision of inputting droplets
  a <- mkDroplet 1
  b <- mkDroplet 1
  c <- mkDroplet 1

  -- ab should have some error, but nothing *more* than we've already incurred
  ab <- mix a b

  -- the split induces more precision loss
  (ab1, ab2) <- split ab

  -- mix again causes no additional error
  abc <- mix ab1 c

  -- we know the below *exactly*! all the precision loss above cancels out
  let v_now = volume abc + volume ab2
  let v_then = volume a + volume b + volume c
  -- assert v_now = v_then

  return abc


-- here is the same program as above, but with a "safe" split that retries
-- within to get the result within some precision.

-- we could limit the number of retries, and it would produce an unbounded but
-- somewhat "narrower" error profile
safe_split :: VolDroplet -> Double -> IO (VolDroplet, VolDroplet)
safe_split self eps = do
  (d1, d2) <- split self
  let err = abs $ volume d1 - volume d2
  if err < eps
  then return (d1, d2)
  else do
    d <- mix d1 d2
    safe_split d eps

safe_main = do
  a <- mkDroplet 1
  b <- mkDroplet 1
  c <- mkDroplet 1

  ab <- mix a b

  -- the split should induce bounded (if there's no limit to retries) precision los
  (ab1, ab2) <- split ab

  abc <- mix ab1 c

  let v_now = volume abc + volume ab2
  let v_then = volume a + volume b + volume c
  -- assert v_now = v_then

  return abc
