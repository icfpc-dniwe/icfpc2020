module ICFPC2020.Operations where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import ICFPC2020.AST

builtinInc :: Function
builtinInc = fun1 "inc" ByValue $ \(VNumber n) -> [VNumber (n + 1)]

builtinDec :: Function
builtinDec = fun1 "dec" ByValue $ \(VNumber n) -> [VNumber (n - 1)]

builtinAdd :: Function
builtinAdd = fun2 "add" ByValue op
  where op (VNumber a) (VNumber b) = [VNumber (a + b)]
        op _ _ = error "Impossible"

builtinMul :: Function
builtinMul = fun2 "mul" ByValue op

  where op (VNumber a) (VNumber b) = [VNumber (a * b)]
        op _ _ = error "Impossible"

builtinCons :: Function
builtinCons = fun3 "cons" ByName op
  where op x0 x1 x2 = [VAp, VAp, x2, x0, x1]

builtinC :: Function
builtinC = fun3 "c" ByName op
  where op x0 x1 x2 = [VAp, VAp, x0, x2, x1]

builtinB :: Function
builtinB = fun3 "b" ByName op
  where op x0 x1 x2 = [VAp, x0, VAp, x1, x2]

builtinS :: Function
builtinS = fun3 "s" ByName op
  where op x0 x1 x2 = [VAp, VAp, x0, x2, VAp, x1, x2]

builtinI :: Function
builtinI = fun1 "i" ByName pure

builtinCar :: Function
builtinCar = fun1 "car" ByName op
  where op x2 = [VAp, x2, valT]

builtinCdr :: Function
builtinCdr = fun1 "cdr" ByName op
  where op x2 = [VAp, x2, valF]

builtinLt :: Function
builtinLt = fun2 "lt" ByValue op
  where op (VNumber x) (VNumber y) = [valCheck (x < y)]
        op a b = error $ show a ++ " and " ++ show b ++ " are not comparable"

builtinNeg :: Function
builtinNeg = fun1 "neg" ByValue op
  where op (VNumber x) = [VNumber $ negate x]
        op _ = error "Impossible"

builtinDiv :: Function
builtinDiv = fun2 "div" ByValue op
  where op (VNumber x) (VNumber y) = [VNumber (x `div` y)]
        op _ _ = error "Impossible"

builtinEq :: Function
builtinEq = fun2 "eq" ByValue op
  where op (VNumber x) (VNumber y) = [valCheck (x == y)]
        op a b = error $ show a ++ " and " ++ show b ++ " are not comparable"

builtinIsnil :: Function
builtinIsnil = fun1 "isnil" ByValue op
  where op VNil = [valT]
        op _ = [valF]

builtins :: HashMap ByteString Function
builtins = HM.fromList $ map (\f -> (BS.pack $ funName f, f)) $
           [ builtinInc
           , builtinDec
           , builtinAdd
           , builtinMul
           , builtinT
           , builtinF
           , builtinCons
           , builtinC
           , builtinB
           , builtinS
           , builtinI
           , builtinCar
           , builtinCdr
           , builtinLt
           , builtinNeg
           , builtinDiv
           , builtinEq
           , builtinIsnil
           ]
