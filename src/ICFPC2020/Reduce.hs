module ICFPC2020.Reduce where

import ICFPC2020.AST

applyValue :: [Value] -> Value -> [Value]
applyValue (VFunction f : arg : stack) VAp =
  case funApply f arg of
    Nothing -> [VAp] ++ funRepresent f ++ [arg] ++ stack
    Just res -> res : stack
applyValue stack value = value : stack

runProgram :: [Value] -> [Value]
runProgram values = foldr (flip applyValue) [] values
