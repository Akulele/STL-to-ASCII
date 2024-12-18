{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status
import Network.Wai.Parse (FileInfo(..))
import Network.Wai.Middleware.Static
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import STLProcessor (processSTL)
import Control.Monad.IO.Class (liftIO)

data AsciiResponse = AsciiResponse 
    { ascii :: String
    , responseStatus :: String 
    } deriving (Show, Generic)

instance ToJSON AsciiResponse
instance FromJSON AsciiResponse

main :: IO ()
main = do
    putStrLn "Starting STL to ASCII server on port 3000..."
    let publicPath = "/home/george/Projects/STL-to-ASCII/public"
    scotty 3000 $ do
        -- Serve static files from the public directory
        middleware $ staticPolicy (noDots >-> addBase publicPath)
        
        middleware $ cors $ const $ Just CorsResourcePolicy
            { corsOrigins = Nothing
            , corsMethods = ["GET", "POST", "OPTIONS"]
            , corsRequestHeaders = ["Content-Type"]
            , corsExposedHeaders = Nothing
            , corsMaxAge = Nothing
            , corsVaryOrigin = True
            , corsRequireOrigin = False
            , corsIgnoreFailures = False
            }
            
        get "/" $ file (publicPath ++ "/index.html")
            
        post "/convert" $ do
            files <- files
            case files of
                [] -> do
                    status status400
                    json $ AsciiResponse "" "No file uploaded"
                (_, fileInfo):_ -> do
                    let content = fileContent fileInfo
                    case processSTL content of
                        Right asciiArt -> 
                            json $ AsciiResponse asciiArt "success"
                        Left err -> do
                            status status400
                            json $ AsciiResponse "" ("Error: " ++ show err)
            
        notFound $ do
            status status404
            json $ AsciiResponse "" "Route not found"