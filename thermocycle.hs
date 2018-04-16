{-@ LIQUID "--no-termination" @-}

module PcrRehydrate where

-- TODO can i make the quanta a constant?
{-@ type AtLeastQuanta = {v: Double | 0.9 < v}  @-}

{-@
data VolDroplet = VolDroplet
  { volume :: AtLeastQuanta }
@-}
data VolDroplet = VolDroplet
  { volume :: Double }

---------------------------------------------------------------------------------------------
-- Data Structures and Helper Functions
---------------------------------------------------------------------------------------------

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

---------------------------------------------------------------------------------------------
-- Thermocycle
---------------------------------------------------------------------------------------------

-- heat will return the same droplet with somewhat (not sure how to calculate
-- yet) lower volume because of evaporation
heat :: VolDroplet -> IO VolDroplet
heat sample = undefined


-- NOTE: it'd be great if dispense only returned a single droplet, and the
-- "remaining" solution were somehow kept track of in the type. This would
-- require that the user can "name" substances, which could be pretty cool

-- Dispense is also assuming more accurate splitting than the hardware is
-- currently capable of
{-@ dispense :: v: {_ | v > 0.9} -> d: {_ | volume d > 0.9 + v} -> _ @-}
dispense :: Double -> VolDroplet -> IO (VolDroplet, VolDroplet)
dispense target_volume sample = return (d1, d2)
  where
      d1 = sample { volume = target_volume }
      d2 = sample { volume = (volume sample) - target_volume }


-- This will heat up a droplet and then restore any lost volume
-- another way to do it would be to calculate how much would be lost, and
-- replenish first, so that the resulting droplet is still above the quanta
-- size
heat_and_replenish :: VolDroplet -> VolDroplet -> IO (VolDroplet, VolDroplet)
heat_and_replenish sample buffer = do
  let v0 = volume sample
  s <- heat sample
  (b, b_rest) <- dispense (v0 - volume s) buffer
  replenished <- mix s b
  return (replenished, b_rest)

-- heat and replenish multiple times for e.g. PCR
thermocycle 0 sample buffer = return sample
thermocycle n sample buffer = do
  (s, b) <- heat_and_replenish sample buffer
  thermocycle (n-1) s b
