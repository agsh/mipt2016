{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, ($//), (&|), (&//), (&/), (>=>)) 
import Network (withSocketsDo)

-- почтовый адрес
email = ""

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- withSocketsDo $ simpleHttp u
     return $ fromDocument $ parseLBS page

lab3 :: IO [T.Text]
lab3 = do
  cursor <- cursorFor "http://mipt.ru/diht/bases/"
  return $ cursor $// element "ul" >=> attributeIs "class" "right-menu" &// element "a" >=> child &| T.concat . content

main :: IO()
main = withSocketsDo $ do
  nodes <- lab3
  dir <- getCurrentDirectory
  initReq <- parseUrl "http://91.239.143.158:13666/lab3"
  handle <- openFile (dir ++ "/Lab2.hs") ReadMode
  hSetEncoding handle utf8_bom
  content <- hGetContents handle
  let req = urlEncodedBody [("email", email), ("result", encodeUtf8 $ T.concat $ nodes), ("content", encodeUtf8 $ T.pack content) ] $ initReq { method = "POST" }
  response <- withManager $ httpLbs req
  hClose handle
  L.putStrLn $ responseBody response
