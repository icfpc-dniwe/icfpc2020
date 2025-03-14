import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Data.Attoparsec.ByteString.Lazy (Result(..), parse)

import ICFPC2020.AST
import ICFPC2020.IO
import ICFPC2020.Reduce
import ICFPC2020.Operations
import ICFPC2020.Generate

main = catch (
    do  
        input <- BL.readFile $ "data/galaxy.txt"
        rawProblem <-
          case parse parseProgram input of
            Done _ r -> return r
            Fail _ ctx e -> fail ("Failed to parse in " ++ show ctx ++ ": " ++ e)
        --putStr "lolol "
        --let !problem = simplifyProgram rawProblem
        --let galaxy = evalMacro problem "galaxy"
        --putStr "hoho "
        --print galaxy
        --let result1 = evalOneExpression problem ([VAp, VAp] ++ galaxy ++ [VNil, VAp, VAp, VFunction builtinCons, VNumber 0, VNumber 0])
        --putStr "hihi "
        --print result1

        let lns = [ "module ICFPC2020.Generated (galaxy) where" :: BS.ByteString,
                      "import ICFPC2020.AST" :: BS.ByteString,
                      "import ICFPC2020.GeneratedOperations" :: BS.ByteString,
                      generateProgram rawProblem ]
        BS.writeFile "src/ICFPC2020/Generated.hs" $ BS.intercalate "\n" lns

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
