module ICFPC2020.Reduce where

import qualified Data.HashMap.Strict as HM

import ICFPC2020.AST
import ICFPC2020.Operations

evalExpression :: GlobalState -> [Value] -> [Value]
evalExpression state (VAp : expr) = evalExpression state (funApply f aValue ++ expr3)
  where fValue : expr2 = evalExpression state expr
        f = case fValue of
              VFunction sf -> sf
              VNil -> builtinNil
              _ -> error "Impossible"
        aValue : expr3 = evalExpression state expr2
evalExpression state (VMacro name : expr) = evalExpression state (macros state HM.! name ++ expr)
evalExpression _ expr = expr
