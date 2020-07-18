module ICFPC2020.Operations where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import ICFPC2020.AST

fun1 :: String -> (Value -> [Value]) -> Function
fun1 name f = ret
  where ret = Function { funName = name
                       , funApply = f
                       }

funArg :: String -> (Value -> Function) -> Function
funArg name constr = ret
  where ret = fun1 name $ \arg0 -> let f = constr arg0
                                   in [VFunction $ f { funName = "ap " ++ funName f ++ " " ++ show arg0 }]

fun2 :: String -> (Value -> Value -> [Value]) -> Function
fun2 name f = funArg name $ \arg0 -> fun1 name $ \arg1 -> f arg0 arg1

fun3 :: String -> (Value -> Value -> Value -> [Value]) -> Function
fun3 name f = funArg name $ \arg0 -> fun2 name $ \arg1 arg2 -> f arg0 arg1 arg2

builtinInc :: Function
builtinInc = fun1 "inc" $ \(VNumber n) -> [VNumber (n + 1)]

builtinDec :: Function
builtinDec = fun1 "dec" $ \(VNumber n) -> [VNumber (n - 1)]

builtinAdd :: Function
builtinAdd = fun2 "add" op
  where op a (VNumber 0) = [a]
        op (VNumber 0) b = [b]
        op (VNumber a) (VNumber b) = [VNumber (a + b)]
        op _ _ = error "Impossible"

builtinMul :: Function
builtinMul = fun2 "mul" op

  where op _ (VNumber 0) = [VNumber 0]
        op (VNumber 0) _ = [VNumber 0]
        op a (VNumber 1) = [a]
        op (VNumber 1) b = [b]
        op (VNumber a) (VNumber b) = [VNumber (a * b)]
        op _ _ = error "Impossible"

builtinT :: Function
builtinT = fun2 "t" (\a _ -> [a])

builtinF :: Function
builtinF = fun2 "f" (\_ b -> [b])

valT :: Value
valT = VFunction builtinT

valF :: Value
valF = VFunction builtinF

-- A bit magical -- it's injected into stack when `ap nil` is encountered, otherwise it's not used.
builtinNil :: Function
builtinNil = fun1 "nil" (\_ -> [valT])

builtinCons :: Function
builtinCons = fun3 "cons" op
  where op x0 x1 x2 = [VAp, VAp, x2, x0, x1]

builtinC :: Function
builtinC = fun3 "c" op
  where op x0 x1 x2 = [VAp, VAp, x0, x2, x1]

builtinB :: Function
builtinB = fun3 "b" op
  where op x0 x1 x2 = [VAp, x0, VAp, x1, x2]

builtinI :: Function
builtinI = fun1 "i" (:[])

builtinCar :: Function
builtinCar = fun1 "car" op
  where op x2 = [VAp, x2, valT]

builtinCdr :: Function
builtinCdr = fun1 "cdr" op
  where op x2 = [VAp, x2, valF]

builtinLt :: Function
builtinLt = fun2 "lt" op
  where op (VNumber x) (VNumber y) | x < y = [valT]
                                   | otherwise = [valF]
        op _ _ = error "Impossible"

builtinNeg :: Function
builtinNeg = fun1 "neg" op
  where op (VNumber x) = [VNumber $ negate x]
        op _ = error "Impossible"

builtinDiv :: Function
builtinDiv = fun2 "div" op
  where op x0 (VNumber 1) = [x0]
        op (VNumber x) (VNumber y) = [VNumber (x `div` y)]
        op _ _ = error "Impossible"

builtins :: HashMap ByteString Function
builtins = HM.fromList $ map (\f -> (BS.pack $ funName f, f)) $
           [ builtinInc
           , builtinDec
           , builtinAdd
           , builtinMul
           , builtinT
           , builtinF
           , builtinNil
           , builtinCons
           , builtinC
           , builtinB
           , builtinI
           , builtinCar
           , builtinCdr
           , builtinLt
           , builtinNeg
           , builtinDiv
           ]
