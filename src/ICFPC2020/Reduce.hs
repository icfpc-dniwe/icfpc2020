module ICFPC2020.Reduce where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

import ICFPC2020.AST

-- A bit magical -- it's injected when `ap nil` is encountered, otherwise it's not used.
builtinNil :: Function
builtinNil = fun1 "nil" ByName (\_ -> [valT])

evalExpressionNoMacro :: Program -> [Value] -> [Value]
evalExpressionNoMacro prog (VAp : expr) = evalExpression prog (funApply f aValue ++ expr3)
  where fValue : expr2 = evalExpression prog expr
        f = case fValue of
              VFunction sf -> sf
              VNil -> builtinNil
              _ -> error $ show fValue ++ " is not a function"
        aValue : expr3 =
         case funStrategy f of
            ByValue -> evalExpression prog expr2
            ByName -> evalExpressionNoMacro prog expr2
evalExpressionNoMacro _ expr = expr

evalExpression :: Program -> [Value] -> [Value]
evalExpression prog (VMacro name : expr) =
  case HM.lookup name (macros prog) of
    Just m -> evalExpression prog (m ++ expr)
    Nothing -> error $ "Macro " ++ BS.unpack name ++ " not found"
evalExpression prog expr = evalExpressionNoMacro prog expr

evalMacro :: Program -> ByteString -> [Value]
evalMacro prog name = evalExpression prog (macros prog HM.! name)
