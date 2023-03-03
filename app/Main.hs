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
import Data.Aeson.Casing



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
    } deriving (Show,Generic,ToJSON,FromJSON)


data Choice = Choice
    { message :: ChoiceMsg
    , index :: Int
    , finishReason :: String
    } deriving (Show,Generic)

instance ToJSON Choice where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase
instance FromJSON Choice where
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase




data ChoiceMsg = ChoiceMsg
    { role :: String
    , content :: String
    } deriving (Show,Generic,ToJSON, FromJSON)

data Usage = Usage
    { promptTokens :: Int
    , completionTokens :: Int
    , totalTokens :: Int
    } deriving (Show,Generic)

instance ToJSON Usage where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase
instance FromJSON Usage where
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase



messageBody :: String -> Data.Aeson.Array
messageBody msg = fromList[
                        Data.Aeson.object[ "role" .= ("system"::String),
                        "content" .= ("You are a helpful assistant."::String)],
                        Data.Aeson.object[ "role" .= ("user"::String ),
                       "content" .= (msg::String)]
                                 ]::Vector Value
                

jsonBody ::  String -> Data.Aeson.Value
jsonBody msg = Data.Aeson.object [
                   "model" .= ("gpt-3.5-turbo-0301"::String)
                   ,"messages" .= ((messageBody msg))
                   --,"max_tokens" .= (4096::Int)
                   ,"top_p" .= (1::Double)
                   ,"temperature" .= (0.8::Double) 
                   ,"presence_penalty" .= (0.6::Double)
                   ]
           

askGPT :: String -> IO (Either Text Text)
askGPT msg = do
    let requestUrl = parseRequest_ (url)
    let bodyStr = encode (jsonBody msg)
    let request = setRequestProxy (Just (Proxy "127.0.0.1" 7890))
            $ setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Authorization" apiKey
          --  $ setRequestBodyJSON (jsonBody msg)
            $ setRequestBodyLBS bodyStr
            $ requestUrl
    -- print request
    -- print bodyStr
    response <- httpLBS request
    let json = getResponseBody response
    print json
    let eitherResult = (eitherDecode $ json) :: Either String ChatGPTResponse
    case eitherResult of
        Right result -> do
            let answer = content . message . Prelude.head . choices $ result  
            return $ Right $ pack answer
        Left error -> do
            print error
            return $ Left $ L.toStrict . decodeUtf8 $ json
            


main :: IO ()
main = do
    putStrLn "Type your question: "
    input <- getLine
    response <- askGPT input
    case response of
        Left err -> do
            putStr "API error: "
            putStrLn $ unpack err
        Right resp -> do
            putStr "AI response: "
            putStrLn $ unpack resp
