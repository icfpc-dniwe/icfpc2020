import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)

import ICFPC2020.AST
import ICFPC2020.IO
import ICFPC2020.Reduce
import ICFPC2020.Operations

main = catch (
    do  
        input <- BL.readFile $ "data/galaxy.txt"
        problem <-
          case parse parseProgram input of
            Done _ r -> return r
            Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
        let galaxy = evalMacro problem "galaxy"
        print galaxy
        let result = evalExpression problem ([VAp, VAp] ++ galaxy ++ [VNil, VAp, VAp, VFunction builtinCons, VNumber 0, VAp, VAp, VFunction builtinCons, VNumber 0, VNil])
        print result

        --args <- getArgs
        --putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
        --request' <- parseRequest ("POST " ++ (args!!0))
        --let request = setRequestBodyLBS (BLU.fromString (args!!1)) request'
        --response <- httpLBS request
        --let statuscode = show (getResponseStatusCode response)
        --case statuscode of
        --    "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
        --    _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
