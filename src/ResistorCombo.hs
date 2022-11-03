-- | Synthesis of electric resistor combinations, approximating a given resistance.
{-# LANGUAGE BlockArguments #-}
module ResistorCombo (resistorCombos) where

import Data.Functor
import Data.List
import AssocCommutFormulae

-- | For each number of resistors, find their best serial-parallel combination
-- to match the wanted resistance as close as possible. Current implementation
-- is brute force so it make take some time if there are many available values
-- of resistors. See the comment in the test for further explanation of the
-- API. The function returns an infinite list of:
-- - The formula describing the chosen serial-parallel connection (Op 0 is
-- serial, Op 1 is parallel).
-- - The resulting resistance of the composition.
-- - The error (resulting minus wanted resistance)
-- - The values of the elementary resistors (in the order in which they appear
-- in the formula)
resistorCombos :: Double -> [Double] -> [(Formula, Double, Double, [Double])]
resistorCombos wanted availables = map findBest (tail steps)
  where
    steps = generateAll 2
    findBest formulas =
      minimumBy
        (\(_, _, err1, _) (_, _, err2, _) -> compare (abs err1) (abs err2))
        $ flip concatMap formulas \formula ->
            evaluations formula <&> \(val, choice) ->
              (formula, val, val - wanted, choice)

    evaluations Leaf = map (\x -> (x, [x])) availables
    evaluations (Op 0 x y) = do
      (a, achoice) <- evaluations x
      (b, bchoice) <- evaluations y
      return (a + b, achoice ++ bchoice)
    evaluations (Op _ x y) = do
      (a, achoice) <- evaluations x
      (b, bchoice) <- evaluations y
      return (a * b / (a + b), achoice ++ bchoice)
