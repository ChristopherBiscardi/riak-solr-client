{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Network.Riak.Solr
-- Copyright:   (c) 2014 Christopher Biscardi
-- License:     MIT
-- Maintainer:  Christopher Biscardi <chris@christopherbiscardi.com>
-- Stability:   experimental
-- Portability: portable
--
-- A client for Riak Search 2 (Yokozuna/Solr)


module Network.Riak.Solr
       (rawQuery
         ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString              as S
import           Data.Text                    (Text)
import           Network.Http.Client
import           Network.Riak.Solr.Types
import           System.IO.Streams            (InputStream, OutputStream,
                                               stdout)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec (parseFromStream)

data Location = Location {
  name :: Text,
  loc  :: Text} deriving Show
instance FromJSON Location where
  parseJSON (Object o) = Location <$> o .: "name"
                                  <*> o .: "loc"
-- just a test
testSimpleGet = do
  d <- locSimpleGet geoURI
  return $ fromResult d

fromResult :: Result a -> a
fromResult (Error m) = error m
fromResult (Success n) = n

-- just a version with a static Location type for playing
locSimpleGet :: URL -> IO (Result (SolrResponse Location))
locSimpleGet uri = get uri resultFromStream

-- TODO: Replace `get` with a `rawQuery` implementation
simpleGet :: FromJSON a => URL -> IO (Result (SolrResponse a))
simpleGet uri = get uri resultFromStream

resultFromStream :: FromJSON a =>
                    t ->
                    InputStream S.ByteString ->
                    IO (Result (SolrResponse a))
resultFromStream _ i = parseJSONFromStream i
-- Meant to be a more flexible implementation
--  for connection pooling in snaplets and such)
rawQuery :: IO ()
rawQuery = do
  c <- openConnection "192.168.50.3" 8098

  q <- buildRequest $ do
         http GET "/"
         setAccept "application/json"

  sendRequest c q emptyBody

  receiveResponse c (\p i -> do
                        xm <- Streams.read i
                        case xm of
                          Just x    -> S.putStr "success"
                          Nothing   -> S.putStr "nothing")

  closeConnection c

-- Utility Function for parsing JSON from Stream
parseJSONFromStream :: FromJSON a =>
                       Streams.InputStream S.ByteString ->
                       IO (Result (SolrResponse a))
parseJSONFromStream = parseFromStream $ fmap fromJSON json

-- Testing Code
-- Should be removed and placed into a "Real"
--  test file with QuickCheck and such.

-- These work. That is all.
y' = get "http://192.168.50.3:8098" (\_ i -> Streams.connect i stdout)
y = get "http://192.168.50.3:8098" debugHandler

-- Static URI for returning a geo query
-- Explained at:
-- http://www.christopherbiscardi.com/2014/02/07/geospatial-indexing-with-riak-search-2-0-yokozunasolr/
-- Also, brackets must be encoded
geoURI = "http://192.168.50.3:8098/search/my_geo_index?&fl=*,score&sort=score%20asc&q=%7B!geofilt%20score=distance%20filter=false%20sfield=loc%20pt=37.441883,-122.143019%20d=10%7D&wt=json"
