-- |
-- Module:      Network.Riak.Solr
-- Copyright:   (c) 2014 Christopher Biscardi
-- License:     MIT
-- Maintainer:  Christopher Biscardi <chris@christopherbiscardi.com>
-- Stability:   experimental
-- Portability: portable
--
-- A client for Riak Search 2 (Yokozuna/Solr)
{-# LANGUAGE OverloadedStrings #-}

module Network.Riak.Solr
       (mkReq
         ) where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import Network.Http.Client

mkReq = do
  c <- openConnection "www.google.com" 80

  q <- buildRequest $ do
         http GET "/"
         setAccept "text/html"

         sendRequest c q emptyBody

         receiveResponse c (\p i -> do
                               xm <- Streams.read i
                                     case xm of
                                       Just x    -> S.putStr x
                                       Nothing   -> S.putStr "nothing")

         closeConnection c

parseJSONFromStream :: FromJSON a => Streams.InputStream S.ByteString -> IO (Result a)
parseJSONFromStream = parseFromStream $ fmap fromJSON json
