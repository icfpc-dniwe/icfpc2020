module ICFPC2020.AST where

data Token = TNumber !Int
           | TVariable !String
           | TOpenList
           | TComma
           | TCloseList
           deriving (Show, Eq)

data Value = VNumber !Int
           | VVariable !String
           | VList ![Value]
           | VFunction !Function
           | VAp

type Declaration = (String, Value)

instance Show Value where
  show (VNumber a) = show a
  show (VFunction f) = funName f
  show (VVariable v) = v
  show (VList vs) = show vs
  show VAp = "ap"

data Function = Function { funName :: !String
                         , funApply :: !(Value -> Maybe Value)
                         , funRepresent :: [Value]
                         }
