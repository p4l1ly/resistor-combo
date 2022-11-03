module Main (main) where

import ResistorCombo
import Control.Monad
import System.Exit

assert :: Bool -> IO ()
assert = (`unless` exitFailure)


main :: IO ()
main = do
  -- We want to assemble a 43.48kOhm resistor but we have only these values:
  -- 100Ohm, 1kOhm, 10kOhm, 100kOhm, 1MOhm.
  -- If we used 1 resistor, the closest match is to use a 10kOhm one
  -- (with error -33.48kOhm).
  -- If we used 2 resistors, a parallel combination of two 100kOhm resistors
  -- is the best.
  -- ...
  -- If we used 4 resistors, a 10kOhm in series with the parallel combination
  -- of three 100kOhm resistors is closest to the desired value (with error
  -- only 148Ohm).
  -- If we used 5 resistors, the parallel combination of two 100kOhm ones and
  -- three 1MOhm ones is the best, with a really negligible error.
  assert $
    map show (take 5 (resistorCombos 43.48 [0.1, 1, 10, 100, 1000]))
    ==
    [ "(.,10.0,-33.48,[10.0])"
    , "(.1.,50.0,6.520000000000003,[100.0,100.0])"
    , "(.1.1.,47.61904761904761,4.139047619047616,[100.0,100.0,1000.0])"
    , "(.0(.1.1.),43.333333333333336,-0.14666666666666117,[10.0,100.0,100.0,100.0])"
    , "(.1.1.1.1.,43.47826086956522,-1.7391304347782466e-3,[100.0,100.0,1000.0,1000.0,1000.0])"
    ]
