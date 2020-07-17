import System.Environment
import Network.HTTP.Simple
import Data.ByteString.Lazy.UTF8 as BLU
import Control.Exception

main = catch (
    do  
        args <- getArgs
        putStrLn ("Number of arguments: " ++ show (Prelude.length args))
        putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
        request' <- parseRequest ("POST " ++ show 110110000111011111100001001111110101000000)
        let request = setRequestBodyLBS (BLU.fromString (args!!1)) request'
        response <- httpLBS request
        let statuscode = show (getResponseStatusCode response)
        case statuscode of
            "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
            _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    ) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
