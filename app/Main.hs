{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.HTTP.Simple
import Data.Aeson
import Data.Text
import Data.Maybe
import Data.Text (unpack, head)
import GHC.Generics
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy  as  L
import Data.ByteString.UTF8 as BSU  hiding(decode)    
import qualified Data.ByteString.Lazy as LB

import qualified Data.Aeson(object,Array)
import Data.Vector




url :: String
url = "https://api.openai.com/v1/chat/completions"

apiKey :: [ByteString]
apiKey =  [ BSU.fromString $ "Bearer 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"]


data ChatGPTResponse = ChatGPTResponse
    { id :: String
    , object :: String
    , created :: Integer
    , model :: String
    , choices :: [Choice]
    , usage :: Usage
    } deriving (Show,Generic,ToJSON, FromJSON)


data Choice = Choice
    { text :: String
    , index :: Int
    , finishReason :: String
    --, logprobs :: Maybe ()
    } deriving (Show,Generic,ToJSON, FromJSON)

data Usage = Usage
    { promptTokens :: Int
    , completionTokens :: Int
    , totalTokens :: Int
    } deriving (Show,Generic,ToJSON, FromJSON)



data MsgItem = MsgItem{
    role :: String,
    content :: String
    } deriving (Show,Generic,ToJSON, FromJSON)


messageBody :: String -> Data.Aeson.Array
messageBody msg = fromList[
                        Data.Aeson.object[ "role" .= ("system"::String),
                        "content" .= ("You are a helpful assistant."::String)],
                        Data.Aeson.object[ "role" .= ("user"::String ),
                       "content" .= (msg::String)]
                                 ]::Vector Value

askGpt :: String -> IO (Either Text Text)
askGpt msg = do
    let requestUrl = parseRequest_ (url)
    let request = setRequestProxy (Just (Proxy "127.0.0.1" 7890))
            $ setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Authorization" apiKey
            $ setRequestHeader "OpenAI-Organization"  ["org-QHLVHOTGsqSzeOLYeAu4jbpP"]
            $ setRequestBodyJSON (Data.Aeson.object [
                   "model" .= ("gpt-3.5-turbo-0301"::String),
                   "messages" .= ((messageBody msg):: Data.Aeson.Array),
                   "max_tokens" .= (512::Int),
                   "top_p" .= (1::Double),
                   "stop" .= (['\n']::String) ,
                   "temperature" .= (0::Double) ,
                   "presence_penalty" .= (0.6::Double)])
            $ requestUrl
    response <- httpLBS request
    let json = getResponseBody response
    let maybeResult = decode $ json :: Maybe ChatGPTResponse
    case maybeResult of
        Just result -> do
            let answer = text . Prelude.head . choices $ result  
            return $ Right $ pack answer
        Nothing -> do
            return $ Left $ L.toStrict . decodeUtf8 $ json
            


main :: IO ()
main = do
    putStrLn "Type your question: "
    input <- getLine
    response <- askGpt input
    case response of
        Left err -> do
            putStr "API error : "
            putStrLn $ unpack err
        Right resp -> do
            putStr "AI response:"
            putStrLn $ unpack resp
