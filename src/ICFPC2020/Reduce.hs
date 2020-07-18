module ICFPC2020.Reduce where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader

import ICFPC2020.AST

import Debug.Trace

-- A bit magical -- it's injected when `ap nil` is encountered, otherwise it's not used.
builtinNil :: Function
builtinNil = fun1 "nil" (\_ -> return [valT])

expandValue :: Value -> Eval Value
expandValue val = do
  ret <- evalExpression [val]
  case ret of
    [ret1] -> return ret1
    rets -> error $ "expanding " ++ show val ++ " returned " ++ show rets

evalExpressionNoMacro :: [Value] -> Eval [Value]
evalExpressionNoMacro (VAp : expr) = do
  expr2 <- evalExpression expr
  let (fValue, expr3) =
        case expr2 of
          fValue' : expr3' -> (fValue', expr3')
          _ -> error $ show expr2 ++ " doesn't contain enough arguments for ap"
  let f = case fValue of
            VFunction sf -> sf
            VNil -> builtinNil
            _ -> error $ show fValue ++ " is not a function"
  expr4 <- evalExpressionNoMacro expr3
  let (arg, expr5) =
        case expr4 of
          arg' : expr5' -> (arg', expr5')
          _ -> error $ show expr2 ++ " doesn't contain enough arguments for ap"
  res <- funApply f arg
  evalExpression $ trace ("reducing " ++ funName f ++ " to " ++ show res) (res ++ expr5)
evalExpressionNoMacro expr = return expr

evalExpression :: [Value] -> Eval [Value]
evalExpression (VMacro name : expr) = do
  prog <- Eval ask
  case HM.lookup name (macros prog) of
    Just m -> evalExpression $ trace ("unfolding " ++ BS.unpack name) (m ++ expr)
    Nothing -> error $ "Macro " ++ BS.unpack name ++ " not found"
evalExpression expr = evalExpressionNoMacro expr

evalOneExpression :: Program -> [Value] -> [Value]
evalOneExpression prog expr = runReader (runEval $ evalExpression expr) prog

evalMacro :: Program -> ByteString -> [Value]
evalMacro prog name = evalOneExpression prog (macros prog HM.! name)
