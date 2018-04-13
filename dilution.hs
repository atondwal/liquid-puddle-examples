{-# LANGUAGE MultiWayIf #-}
{-@ LIQUID "--no-termination" @-}

module Dilution where

import Prelude hiding (abs)

{-@ inline abs @-}
abs :: Double -> Double
abs z = if z >= 0 then z else (0 - z)

---------------------------------------------------------------------------------------------
-- Aliases for Writing Specs
---------------------------------------------------------------------------------------------
{-@ type PosDouble            = {v:Double | 0 < v}  @-}
{-@ type DropletRng     Lo Hi = {v:_ | Lo <= concentration v && concentration v <= Hi} @-}
{-@ type DropletBetween D1 D2 = DropletRng (concentration D1) (concentration D2)       @-}
{-@ type DropletConc     C    = DropletRng C C                                         @-}
{-@ type DropletLike     D    = DropletConc (concentration D)                          @-}
{-@ type DropletConcEps  C E Lo Hi = {v:_ | isDilution Lo Hi (concentration v)
                                         && isApprox   C E   (concentration v) }       @-}

{-@ inline weaker @-}
weaker :: VolConcDroplet -> VolConcDroplet -> Bool
weaker d1 d2 = concentration d1 <= concentration d2

{-@ inline isDilution @-}
isDilution :: DropletFactory -> DropletFactory -> Double -> Bool
isDilution lo hi v = conc lo <= v && v <= conc hi

{-@ inline isApprox @-}
isApprox :: Double -> Double -> Double -> Bool
isApprox c eps v = abs (v - c) < eps

---------------------------------------------------------------------------------------------
-- Data Structures and Helper Functions
---------------------------------------------------------------------------------------------
data Session        = Session

{-@ data VolConcDroplet = VolConcDroplet
      { volume        :: PosDouble
      , concentration :: Double
      }
  @-}
data VolConcDroplet = VolConcDroplet
  { volume        :: Double
  , concentration :: Double
  }

{-@ data DropletFactory = DF
      { df_conc :: Double
      , df_new  :: () -> IO (DropletConc df_conc)
      }
  @-}
data DropletFactory = DF
  { df_conc :: Double
  , df_new  :: () -> IO VolConcDroplet
  }

flush :: Session -> IO ()
flush s = return ()

{-@ inline mass @-}
mass :: VolConcDroplet -> Double
mass d = concentration d * volume d

{-@ mix :: _ -> d0:_ -> d1:{_ | weaker d0 d1} -> IO (DropletBetween d0 d1) @-}
mix :: Session -> VolConcDroplet -> VolConcDroplet -> IO VolConcDroplet
mix _ self other  = return $ VolConcDroplet
  { volume        = result_volume
  , concentration = (mass1 + mass2) / result_volume
  }
  where
    result_volume = volume self + volume other
    mass1         = mass self
    mass2         = mass other

{-@ split :: _ -> d:_ -> IO (DropletLike d, DropletLike d) @-}
split :: Session -> VolConcDroplet -> IO (VolConcDroplet, VolConcDroplet)
split _ self = return (d1, d2)
  where
      d1     = self { volume = (volume self) / 2 }
      d2     = self { volume = (volume self) / 2 }

{-@ measure conc @-}
conc :: DropletFactory -> Double
conc (DF c _) = c

{-@ new :: df:DropletFactory -> () -> IO (DropletConc (conc df)) @-}
new :: DropletFactory -> () -> IO VolConcDroplet
new (DF c f) = f

---------------------------------------------------------------------------------------------
-- Dilution
---------------------------------------------------------------------------------------------
{-@ dilute :: _ -> lo:_ -> hi:_ -> c:{_ | isDilution lo hi c} -> e:_
           -> IO (DropletConcEps c e lo hi) @-}
dilute :: Session
       -> DropletFactory
       -> DropletFactory
       -> Double
       -> Double
       -> IO VolConcDroplet

dilute session d_low_factory d_high_factory c_target epsilon = do

  d_low  <- new d_low_factory ()
  d_high <- new d_high_factory ()

  -- Moved to the precondition
  -- assert (concentration d_low <= c_target)

  dilute_rec d_low d_high

  where

  {-@ dilute_rec :: d0:DropletRng (conc d_low_factory) c_target
                 -> d1:DropletRng c_target (conc d_high_factory)
                 -> _ @-}
  dilute_rec d0 d1 = do

    let con0 = concentration d0
    let con1 = concentration d1
    assert (con0 <= con1)

    if | abs(con0 - c_target) < epsilon -> return d0
       | abs(con1 - c_target) < epsilon -> return d1
       | otherwise                      -> do

       _         <- flush session
       d         <- mix session d0 d1
       (da, db)  <- split session d
       _         <- flush session

       let d_next = da
       let c_next = concentration d_next

       if | abs (c_next - c_target) < epsilon -> return d_next
          | c_next < c_target                 -> do

            d1_again <- dilute session d_low_factory d_high_factory con1 epsilon

            -- Moved to the postcondition, but with epsilon
            -- assert (c_target <= concentration d1_again)
            -- assert (concentration d1_again <= conc d_high_factory)

            dilute_rec d_next d1_again

          | otherwise                         -> do

            d0_again <- dilute session d_low_factory d_high_factory con0 epsilon
            dilute_rec d0_again d_next

---------------------------------------------------------------------------------------------
-- Harness
---------------------------------------------------------------------------------------------
main :: IO ()
main = do
  session <- mk_session "../../tests/arches/arch-big.json"
  d       <- dilute session
               (factory session c_low)
               (factory session c_high)
               c_target
               eps
  assert (abs (concentration d - c_target) < eps)
  return ()
  where
    c_low    = 0
    c_high   = 1
    c_target = 0.37
    eps      = 0.01

{-@ factory :: Session -> c:Double -> {df:DropletFactory | conc df == c} @-}
factory :: Session -> Double -> DropletFactory
factory session c = DF c (\_ -> input session 1 c)

---

{-@ input :: _ -> PosDouble -> c:Double -> IO (DropletConc c) @-}
input :: Session -> Double -> Double -> IO VolConcDroplet
input _ vol conc = return (VolConcDroplet vol conc)

mk_session :: FilePath -> IO Session
mk_session = undefined

{-@ assert :: {p:Bool | p} -> _ @-}
assert :: Bool -> IO ()
assert _ = return ()
