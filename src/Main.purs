module Main where

import Prelude

import ExpectInferred (expectInferred)
import Prim.RowList as RL
import Type.Prelude (BProxy(..), False, Proxy(..), SProxy(..), True, kind Boolean)

class RowContains (label :: Symbol) (row :: # Type) (boolean :: Boolean) | label row -> boolean

instance rowContainsInst ::
  ( RL.RowToList row rl
  , RowContainsImpl label rl boolean
  ) => RowContains label row boolean

class RowContainsImpl (label :: Symbol) (rl :: RL.RowList) (boolean :: Boolean) | label rl -> boolean

instance rowContainsImplNil :: RowContainsImpl label RL.Nil False

else instance rowContainsImplMatch :: RowContainsImpl label (RL.Cons label ty tail) True

else instance rowContainsImplNoMatch ::
  ( RowContainsImpl label tail boolean
  ) => RowContainsImpl label (RL.Cons name ty tail) boolean

rowContains :: forall label proxy row result. RowContains label row result => SProxy label -> proxy row -> BProxy result
rowContains _ _ = BProxy

test1 :: Unit
test1 =
  let
    expected = Proxy :: Proxy (BProxy False)
    actual = rowContains (SProxy :: SProxy "a") {}
  in
    expectInferred expected actual

test2 :: Unit
test2 =
  let
    expected = Proxy :: Proxy (BProxy True)
    actual = rowContains (SProxy :: SProxy "a") { a: 1 }
  in
    expectInferred expected actual
