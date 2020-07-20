module ICFPC2020.GeneratedOperations where

builtinT' x y = x
builtinF' x y = y
builtinInc' = (+1)
builtinDec' = (-1)
builtinAdd' = (+)
builtinMul' = (*)
builtinCons' x y z = (z x) y
builtinC' x y z = (x z) . y
builtinB' x y z = x . (y z)
builtinS' x y z = (x z) . (y z)
builtinI' x = x
builtinCar' x = x . builtinT'
builtinCdr' x = x . builtinF'
builtinLt' x y = if x < y then builtinT' else builtinF'
builtinNeg' = negate
builtinDiv' = (div)
builtinEq' x y = if x == y then builtinT' else builtinF'
builtinIsnil' x = case x of
    [] -> builtinT'
    _ -> builtinF'
