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
import qualified Data.Text.Lazy  as  TL
import qualified Data.Text.Lazy.Encoding    as TL

import Data.ByteString.UTF8 as BSU  hiding(decode)    
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson(object,Array)
import Data.Vector  hiding( (++) )
import Data.Aeson.Casing
import System.Environment
import Configuration.Dotenv
import System.IO
import Data.Char (isSpace)


url :: String
url = "https://api.openai.com/v1/chat/completions"

apiKey :: IO [ByteString]
apiKey = do
    key <- getEnv "api_key"
    return [ BSU.fromString $ "Bearer " ++ key ]


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
    , finish_reason :: String
    } deriving (Show,Generic,ToJSON,FromJSON)



data ChoiceMsg = ChoiceMsg
    { role :: String
    , content :: String
    } deriving (Show,Generic,ToJSON, FromJSON)

data Usage = Usage
    { prompt_tokens :: Int
    , completion_tokens :: Int
    , total_tokens :: Int
    } deriving (Show,Generic,ToJSON,FromJSON)


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
           

checkAndAskGPT :: String -> IO()
checkAndAskGPT msg = do
    let blank = Prelude.all isSpace msg
    if blank
        then return ()
        else askGPT msg

askGPT :: String -> IO ()
askGPT msg = do
    key <- apiKey
    proxyHost <- getEnv "proxy_host"
    proxyPortStr <- getEnv "proxy_port"
    let proxyPort :: Int
        proxyPort = read proxyPortStr
    let requestUrl = parseRequest_ (url)
    let bodyStr = encode (jsonBody msg)
    let request = setRequestProxy (Just (Proxy (BSU.fromString proxyHost) proxyPort ))
            $ setRequestMethod "POST"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestHeader "Authorization" key
          --  $ setRequestBodyJSON (jsonBody msg)
            $ setRequestBodyLBS bodyStr
            $ requestUrl
    
    -- print bodyStr
    response <- httpLBS request
    let json = getResponseBody response
    let eitherResult = (eitherDecode $ json) :: Either String ChatGPTResponse
    case eitherResult of
        Right result -> do
            let answer = content . message . Prelude.head . choices $ result  
            putStrLn answer
            
        Left error -> do
            putStrLn $ TL.unpack . TL.decodeUtf8 $ json
            
            


main :: IO ()
main = do    
    loadFile defaultConfig
    until_ (== "quit") (readPrompt "chatgpt>>> ") checkAndAskGPT
    


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
