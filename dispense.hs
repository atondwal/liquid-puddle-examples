import Control.Monad.State

{-@ type AtLeastQuanta = {v: Double | 0.9 < v}  @-}

{-@
data VolDroplet = VolDroplet
  { volume :: AtLeastQuanta }
@-}
data VolDroplet = VolDroplet
  { volume :: Double }

-- Dispense is also assuming more accurate splitting than the hardware is
-- currently capable of
{-@ dispense :: v: {_ | v > 0.9} -> d: {_ | volume d > 0.9 + v} -> _ @-}
dispense :: Double -> State VolDroplet VolDroplet
dispense target_volume = do
  d <- get
  let dTarget = d { volume = target_volume }
  put $ d { volume = (volume d) - target_volume }
  return dTarget

protocol :: State VolDroplet ()
protocol = do
  _ <- dispense 1
  _ <- dispense 1
  return ()

-- this is safe, but smaller volumes may not be
-- also, how can we make it safe to use up the last bit in the dispenser?
-- right now, you must always leave quanta amount in there for it to be safe
_ = evalState protocol VolDroplet { volume = 3.0}
