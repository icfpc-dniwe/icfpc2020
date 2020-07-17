module ICFPC2020.AST where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader

data Value = VNumber !Int
           | VMacro !ByteString
           | VList ![Value]
           | VFunction !Function
           | VAp
           | VApFun !Function

type Macro = (String, [Value])
type Program = HashMap String [Value]

instance Show Value where
  show (VNumber a) = show a
  show (VFunction f) = BS.unpack $ funName f
  show (VMacro v) = BS.unpack v
  show (VList []) = "nil"
  show (VList vs) = show vs
  show VAp = "ap"
  show (VApFun f) = "ap " ++ BS.unpack (funName f)

data Function = Function { funName :: String
                         , funApply :: !(Value -> [Value])
                         }
