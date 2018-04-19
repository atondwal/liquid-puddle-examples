
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
