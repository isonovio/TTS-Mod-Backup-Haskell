module Network where

import Prelude
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.Char (isAlphaNum)
import Data.ByteString.Lazy as L
-- import Control.Exception as X

contentTypeToExtension :: String -> String
contentTypeToExtension = Prelude.takeWhile isAlphaNum . Prelude.tail . Prelude.dropWhile (/= '/')

downloadFile :: String -> FilePath -> IO String
downloadFile url filePath_ = do
    request <- parseRequest url
    response <- httpLBS request -- `X.catch` statusExceptionHandler
    let statusCode = show $ getResponseStatusCode response
    if statusCode == "200" then do
        let contentType = show $ Prelude.head $ getResponseHeader hContentType response
        let extension = contentTypeToExtension contentType
        let filePath = filePath_ ++ "." ++ extension
        L.writeFile filePath $ getResponseBody response
        putStrLn $ "File downloaded to " ++ filePath
        pure extension
    else do
        putStrLn $ "Failed to retrieve " ++ url
        pure ""
