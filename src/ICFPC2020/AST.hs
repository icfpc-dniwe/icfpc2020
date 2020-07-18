module ICFPC2020.AST where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

data Token = TNumber !Int
           | TMacro !ByteString
           | TOpenList
           | TComma
           | TCloseList
           | TNil
           deriving (Show, Eq)

data Value = VNumber !Int
           | VMacro !ByteString
           | VNil
           | VFunction !Function
           | VAp

instance Show Value where
  show (VNumber a) = show a
  show (VFunction f) = funName f
  show (VMacro v) = BS.unpack v
  show VNil = "nil"
  show VAp = "ap"

data Strategy = ByValue | ByName
              deriving (Show, Eq)

data Function = Function { funName :: String
                         , funApply :: !(Value -> [Value])
                         , funStrategy :: !Strategy
                         }

type Macro = (ByteString, [Value])
data Program = Program { macros :: !(HashMap ByteString [Value])
                       }
             deriving (Show)

emptyProgram :: Program
emptyProgram = Program { macros = HM.empty }

fun1 :: String -> Strategy -> (Value -> [Value]) -> Function
fun1 name strategy f = ret
  where ret = Function { funName = name
                       , funApply = f
                       , funStrategy = strategy
                       }

funArg :: String -> Strategy -> (Value -> Function) -> Function
funArg name strategy constr = ret
  where ret = fun1 name strategy $ \arg0 -> let f = constr arg0
                                         in [VFunction $ f { funName = "ap " ++ funName f ++ " " ++ show arg0 }]

fun2 :: String -> Strategy -> (Value -> Value -> [Value]) -> Function
fun2 name strategy f = funArg name strategy $ \arg0 -> fun1 name strategy $ \arg1 -> f arg0 arg1

fun3 :: String -> Strategy -> (Value -> Value -> Value -> [Value]) -> Function
fun3 name strategy f = funArg name strategy $ \arg0 -> fun2 name strategy $ \arg1 arg2 -> f arg0 arg1 arg2

builtinT :: Function
builtinT = fun2 "t" ByName (\a _ -> [a])

builtinF :: Function
builtinF = fun2 "f" ByName (\_ b -> [b])

valT :: Value
valT = VFunction builtinT

valF :: Value
valF = VFunction builtinF

valCheck :: Bool -> Value
valCheck True = valT
valCheck False = valF
