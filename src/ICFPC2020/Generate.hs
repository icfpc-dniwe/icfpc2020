module ICFPC2020.Generate
  ( generateProgram
  ) where

import qualified Data.Char as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU

import ICFPC2020.AST
import ICFPC2020.Operations

generateProgram :: Program -> ByteString
generateProgram p = BS.intercalate "\n" $ HM.elems $ HM.mapWithKey (\k v -> BS.intercalate " = " [k, v]) $ generateProgram' p

generateProgram' :: Program -> HashMap ByteString ByteString
generateProgram' p = let ms = macros p in
    HM.fromList $ zip (generateString <$> VMacro <$> HM.keys ms) $ generate <$> HM.elems ms

generate :: [Value] -> ByteString
generate ops = head $ generate' (reverse ops) []

generate' :: [Value] -> [ByteString] -> [ByteString]
generate' (VAp:t) (x0:(x1:stack)) = generate' t $ [BS.intercalate " " ["(" :: ByteString, x0, x1, ")" :: ByteString]] ++ stack
generate' (h:t) stack = generate' t $ [generateString h] ++ stack
generate' [] stack = stack

generateString :: Value -> ByteString
generateString VAp = ""
generateString VNil = "[]"
generateString (VNumber a) = BS.intercalate "" ["(" :: ByteString, BU.fromString $ show a, ")" :: ByteString]
generateString (VMacro name) = let name' = BS.unpack name in
    case head name' of
        ':' -> BU.fromString $ "fun_" ++ tail name'
        _ -> name
generateString (VFunction f) = let (h:t) = funName f in
    BS.intercalate "" ["builtin" ::ByteString, BU.fromString ([C.toUpper h] ++ t ++ "'")]