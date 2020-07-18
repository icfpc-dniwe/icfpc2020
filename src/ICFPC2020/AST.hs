module ICFPC2020.AST where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader

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

newtype Eval a = Eval { runEval :: Reader Program a }
               deriving newtype (Functor, Applicative, Monad)

data Function = Function { funName :: String
                         , funApply :: !(Value -> Eval [Value])
                         }

type Macro = (ByteString, [Value])
data Program = Program { macros :: !(HashMap ByteString [Value])
                       }
             deriving (Show)

emptyProgram :: Program
emptyProgram = Program { macros = HM.empty }

fun1 :: String -> (Value -> Eval [Value]) -> Function
fun1 name f = ret
  where ret = Function { funName = name
                       , funApply = f
                       }

nameWithArg :: String -> Value -> String
nameWithArg name arg = name ++ "(" ++ show arg ++ ")"

funArg :: String -> (Value -> Function) -> Function
funArg name constr = ret
  where ret = fun1 name $ \arg0 -> let f = constr arg0
                                   in return [VFunction $ f { funName = nameWithArg (funName f) arg0 }]

fun2 :: String -> (Value -> Value -> Eval [Value]) -> Function
fun2 name f = funArg name $ \arg0 -> fun1 name $ \arg1 -> f arg0 arg1

fun3 :: String -> (Value -> Value -> Value -> Eval [Value]) -> Function
fun3 name f = funArg name $ \arg0 -> fun2 (nameWithArg name arg0) $ \arg1 arg2 -> f arg0 arg1 arg2

builtinT :: Function
builtinT = fun2 "t" (\a _ -> return [a])

builtinF :: Function
builtinF = fun2 "f" (\_ b -> return [b])

valT :: Value
valT = VFunction builtinT

valF :: Value
valF = VFunction builtinF

valCheck :: Bool -> Value
valCheck True = valT
valCheck False = valF
