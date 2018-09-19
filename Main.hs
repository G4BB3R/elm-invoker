{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Builder as DBB
import Data.Monoid
import System.Directory (removeFile)
import Data.Text (Text, unpack, pack, append)

import System.Exit 
import System.Environment (getArgs)
import System.Process (callCommand, readProcess, readCreateProcessWithExitCode, shell, createProcess, proc)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

-- path is expected to be `src/Main.elm`
compile :: String -> IO String
compile path = do
    (exitCode, stdout, stderr) <-
        readCreateProcessWithExitCode
            (shell $ "elm make " ++ path ++ " --output=__result__.js --report=json --debug") ""

    if stderr == "" then do
        let tmpFile = "__result__.js"
        result <- readFile tmpFile
        removeFile tmpFile
        return result
    else
        return $ "const Elm = { Errors: " ++ show stderr ++ " }"

joinPath :: [Text] -> Text 
joinPath [] = ""
joinPath (x : []) = x
joinPath (x : xs) = x `append` "/" `append` joinPath xs


send_js :: Text -> Response
send_js content =
    let
        content_ = DBB.stringUtf8 (Data.Text.unpack content)
    in
    responseBuilder status200 [ ("Content-Type", "text/javascript") ] content_


root_route :: Response
root_route =
    responseBuilder status200 [("Content-Type", "text/html")] 
    "alert('Welcome to Elm Invoker! \\n Use localhost:port/_compile/relative/path/to/Main.elm')"

(|>) = flip ($)

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond =  respond $
    case pathInfo req of
        "_compile" : path ->
            path 
                |> joinPath
                |> Data.Text.unpack
                |> compile
                |> unsafePerformIO
                |> Data.Text.pack
                |> send_js

        _ ->
            root_route
    

getPorts :: [String] -> Int -> IO Int
getPorts [] default_ = return $ default_
getPorts (x:_) default_ =
    case x of 
        '-' : '-' : 'p' : 'o' : 'r' : 't' : '=' : str ->
            return $ (read str :: Int)

        '-' : '-' : 'h' : 'e' : 'l' : 'p' : [] -> 
            die help_text >> return default_

        '-' : '-' : 'v' : 'e' : 'r' : 's' : 'i' : 'o' : 'n' : [] ->
            die "v0.1" >> return default_

        _ ->
            die ("Invalid command!\n" ++ help_text)
                >> return default_
    where
        help_text = "Args:\n--port=8001\n--help\n--version"
    
    

main :: IO ()
main = do
    args <- getArgs
    config_port <- getPorts args 8001
    let (port_live, port_reactor) = (config_port, config_port + 1)
    putStrLn $ "Listening Elm Invoker on port " ++ show port_live ++ " and elm reactor on port " ++ show port_reactor
    _ <- createProcess (proc "elm" ["reactor", "--port=" ++ show port_reactor])
    run port_live app
