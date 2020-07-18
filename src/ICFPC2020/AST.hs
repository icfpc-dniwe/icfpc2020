module ICFPC2020.AST where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Reader

data Value = VNumber !Int
           | VVariable !String
           | VList ![Value]
           | VFunction !Function
           | VAp

type Macro = (String, [Value])
type Program = HashMap String [Value]

instance Show Value where
  show (VNumber a) = show a
  show (VFunction f) = "fun_" ++ funName f
  show (VVariable v) = "var_" ++ v
  show (VList vs) = show vs
  show VAp = "ap"

data Function = Function { funName :: !String
                         , funApply :: !(Value -> Maybe Value)
                         , funRepresent :: [Value]
                         }
