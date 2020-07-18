module ICFPC2020.Reduce where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

import ICFPC2020.AST
import ICFPC2020.Operations

evalExpression :: Program -> [Value] -> [Value]
evalExpression prog (VAp : expr) = evalExpression prog (funApply f aValue ++ expr3)
  where fValue : expr2 = evalExpression prog expr
        f = case fValue of
              VFunction sf -> sf
              VNil -> builtinNil
              _ -> error "Impossible"
        aValue : expr3 = evalExpression prog expr2
evalExpression prog (VMacro name : expr) =
  case HM.lookup name (macros prog) of
    Just m -> evalExpression prog (m ++ expr)
    Nothing -> error $ "Macro " ++ BS.unpack name ++ " not found"
evalExpression _ expr = expr

evalMacro :: Program -> ByteString -> [Value]
evalMacro prog name = evalExpression prog (macros prog HM.! name)
