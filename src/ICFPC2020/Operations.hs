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

fun3 :: String -> (Value -> Value -> Value -> Maybe Value) -> Function
fun3 name f = ret
  where ret = fun2 name $ \arg1 -> Just $ VFunction $ Function { funName = name ++ "_ap"
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

churchTrue :: Function
churchTrue = fun2 "true" op
  where op x _ = Just x
        op _ _ = Nothing

churchFalse :: Function
churchFalse = fun2 "false" op
  where op _ y = Just y
        op _ _ = Nothing

nil :: Value
nil = VList []

cons :: Function
cons = fun2 "cons" op
  where op (VNumber x) (VList ys) = Just . Vlist $ (VNumber x) : ys
        op _ _ = Nothing

combC :: Function  -- TODO:
combC = fun3 "combC" op
  where op (VFunction fun) x y = Just $ fun x y
        op _ _ _ = Nothing

combB :: Function  -- TODO:
combB = fun3 "combB" op
  where op (VFunction fx) (VFunction fy) z = Just $ fx $ fy z
        op _ _ _ = Nothing

combS :: Function  -- TODO:
combS = fun3 "combS" op
  where op (VFunction fx) (VFunction fy) z = Just $ fx z (fy z)
        op _ _ _ = Nothing

combI :: Function
combI = fun1 "combI" Just

car :: Function
car = fun1 "car" op
  where op (VList (x:xs)) = Just x
        op (VFunction fx) = Just (fx churchTrue)  -- TODO:
        op _ _ = Nothing

cdr :: Function
cdr = fun1 "cdr" op
  where op (VList (x:xs)) = Just xs
        op (VFunction fx) = Just $ fx churchFalse  -- TODO:
        op _ _ = Nothing

lessThen :: Function
lessThen = fun2 "lt" op
  where op (VNumber x) (VNumber y) | x < y = Just churchTrue
                                   | otherwise = Just churchFalse
        op _ _ = Nothing

negate :: Function
negate = fun1 "neg" op
  where op (VNumber x) = Just (VNumber -x)
        op _ = Nothing

buildinDiv :: Function
buildinDiv = fun2 "duv" op
  where op (VNumber x) (VNumber y) = Just $ VNumber x + y


