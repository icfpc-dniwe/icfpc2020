module ICFPC2020.Reduce where

import ICFPC2020.AST

data GlobalState = GlobalState { macros :: !(HashMap ByteString [Value])
                               }

newtype Stack

newtype Eval a = Eval { runEval :: StateT (Reader GlobalState) a }
               deriving (Functor, Applicative, Monad)

forceValue :: Value -> Eval Value
forceValue (VMacro name) = Eval $ do
  state <- ask
  return $ macros state HM.! name
forceValue ()

applyValue :: [Value] -> Value -> [Value]
applyValue (VFunction f : arg : stack) VAp =
  case funApply f arg of
    Nothing -> [VAp] ++ funRepresent f ++ [arg] ++ stack
    Just res -> res : stack
applyValue stack value = value : stack

runProgram :: [Value] -> [Value]
runProgram values = foldr (flip applyValue) [] values
