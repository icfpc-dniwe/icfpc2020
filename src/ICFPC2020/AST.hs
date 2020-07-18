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

data Function = Function { funName :: String
                         , funApply :: !(Value -> [Value])
                         }

type Macro = (ByteString, [Value])
data Program = Program { macros :: !(HashMap ByteString [Value])
                       }
             deriving (Show)

emptyProgram :: Program
emptyProgram = Program { macros = HM.empty }
