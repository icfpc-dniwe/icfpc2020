{-# LANGUAGE LambdaCase #-}

module ICFPC2020.Operations where

import ICFPC2020.AST

fun1 :: String -> (Value -> Maybe Value) -> Function
fun1 name f = ret
  where ret = Function { funName = name
                       , funApply = f
                       , funRepresent = [VFunction ret]
                       }

fun2 :: String -> (Value -> Value -> Maybe Value) -> Function
fun2 name f = ret
  where ret = fun1 name $ \arg1 -> Just $ VFunction $ Function { funName = name ++ "_ap"
                                                               , funApply = \arg2 -> f arg1 arg2
                                                               , funRepresent = [VAp, VFunction ret, arg1]
                                                               }

funNumber :: (Int -> Maybe Value) -> (Value -> Maybe Value)
funNumber f (VNumber n) = f n
funNumber _ _ = Nothing

builtinInc :: Function
builtinInc = fun1 "inc" $ funNumber $ \n -> Just $ VNumber (n + 1)

builtinDec :: Function
builtinDec = fun1 "dec" $ funNumber $ \n -> Just $ VNumber (n - 1)

builtinAdd :: Function
builtinAdd = fun2 "add" op
  where op a (VNumber 0) = Just a
        op (VNumber 0) b = Just b
        op (VNumber a) (VNumber b) = Just $ VNumber (a + b)
        op _ _ = Nothing

builtinMul :: Function
builtinMul = fun2 "mul" op
  where op _ (VNumber 0) = Just (VNumber 0)
        op (VNumber 0) _ = Just (VNumber 0)
        op a (VNumber 1) = Just a
        op (VNumber 1) b = Just b
        op (VNumber a) (VNumber b) = Just $ VNumber (a * b)
        op _ _ = Nothing
