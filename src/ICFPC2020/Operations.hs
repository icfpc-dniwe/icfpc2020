module ICFPC2020.Operations where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import ICFPC2020.AST
import ICFPC2020.Reduce

builtinInc :: Function
builtinInc = fun1 "inc" op
  where op value = do
          xV <- expandValue value
          return $ case xV of
            VNumber x -> [VNumber (x + 1)]
            _ -> error "Impossible"

builtinDec :: Function
builtinDec = fun1 "dec" op
  where op value = do
          xV <- expandValue value
          return $ case xV of
            VNumber x -> [VNumber (x - 1)]
            _ -> error "Impossible"

builtinAdd :: Function
builtinAdd = fun2 "add" op
  where op xValue yValue = do
          xV <- expandValue xValue
          yV <- expandValue yValue
          return $ case (xV, yV) of
            (VNumber x, VNumber y) -> [VNumber (x + y)]
            (a, b) -> error $ show a ++ " and " ++ show b ++ " are not numbers"

builtinMul :: Function
builtinMul = fun2 "mul" op
  where op xValue yValue = do
          xV <- expandValue xValue
          yV <- expandValue yValue
          return $ case (xV, yV) of
            (VNumber x, VNumber y) -> [VNumber (x * y)]
            (a, b) -> error $ show a ++ " and " ++ show b ++ " are not numbers"

builtinCons :: Function
builtinCons = fun3 "cons" op
  where op x0 x1 x2 = return [VAp, VAp, x2, x0, x1]

builtinC :: Function
builtinC = fun3 "c" op
  where op x0 x1 x2 = return [VAp, VAp, x0, x2, x1]

builtinB :: Function
builtinB = fun3 "b" op
  where op x0 x1 x2 = return [VAp, x0, VAp, x1, x2]

builtinS :: Function
builtinS = fun3 "s" op
  where op x0 x1 x2 = return [VAp, VAp, x0, x2, VAp, x1, x2]

builtinI :: Function
builtinI = fun1 "i" (return . pure)

builtinCar :: Function
builtinCar = fun1 "car" op
  where op x2 = return [VAp, x2, valT]

builtinCdr :: Function
builtinCdr = fun1 "cdr" op
  where op x2 = return [VAp, x2, valF]

builtinLt :: Function
builtinLt = fun2 "lt" op
  where op xValue yValue = do
          xV <- expandValue xValue
          yV <- expandValue yValue
          return $ case (xV, yV) of
            (VNumber x, VNumber y) -> [valCheck (x < y)]
            (a, b) -> error $ show a ++ " and " ++ show b ++ " are not numbers"

builtinNeg :: Function
builtinNeg = fun1 "neg" op
  where op value = do
          xV <- expandValue value
          return $ case xV of
            VNumber x -> [VNumber $ negate x]
            _ -> error "Impossible"

builtinDiv :: Function
builtinDiv = fun2 "div" op
  where op xValue yValue = do
          xV <- expandValue xValue
          yV <- expandValue yValue
          return $ case (xV, yV) of
            (VNumber x, VNumber y) -> [VNumber (x `div` y)]
            (a, b) -> error $ show a ++ " and " ++ show b ++ " are not numbers"

builtinEq :: Function
builtinEq = fun2 "eq" op
  where op xValue yValue = do
          xV <- expandValue xValue
          yV <- expandValue yValue
          return $ case (xV, yV) of
            (VNumber x, VNumber y) -> [valCheck (x == y)]
            (a, b) -> error $ show a ++ " and " ++ show b ++ " are not comparable"

builtinIsnil :: Function
builtinIsnil = fun1 "isnil" op
  where op value = do
          v <- expandValue value
          return $ case v of
            VNil -> [valT]
            _ -> [valF]

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
