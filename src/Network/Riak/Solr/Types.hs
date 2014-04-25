{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Module:      Network.Riak.Solr.Types
-- Copyright:   (c) 2014 Christopher Biscardi
-- License:     MIT
-- Maintainer:  Christopher Biscardi <chris@christopherbiscardi.com>
-- Stability:   experimental
-- Portability: portable
--
-- A client for Riak Search 2 (Yokozuna/Solr)

module Network.Riak.Solr.Types
       (SolrResponse(..)
       ,Results(..)
       ,DocHolder(..)
       ,Doc(..)
       ,ResponseHeader(..)
       ,Name(..)
       ,getDocHolders
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (mzero)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as M


newtype Params = Params (M.Map String String) deriving (Show)
instance FromJSON Params where
  parseJSON val = Params <$> parseJSON val

data ResponseHeader = ResponseHeader {
  _status :: Int,
  _qTime  :: Int,
  _params :: Params
} deriving (Show)
instance FromJSON ResponseHeader where
  parseJSON (Object o) = ResponseHeader <$> o .: "status"
                                        <*> o .: "QTime"
                                        <*> o .: "params"
  parseJSON _ = mzero

data Doc = Doc {
  __yz_id :: String,
  __yz_rk :: String,
  __yz_rt :: String,
  __yz_rb :: String,
  _score  ::  Maybe Double
} deriving (Show)
instance FromJSON Doc where
  parseJSON (Object o) = Doc <$> o .: "_yz_id"
                             <*> o .: "_yz_rk"
                             <*> o .: "_yz_rt"
                             <*> o .: "_yz_rb"
                             <*> o .:? "score"
  parseJSON _ = mzero

data Results a = Results {
  _numFound :: Int,
  _start    :: Int,
  _maxScore :: Float,
  _docs     :: [DocHolder a]
} deriving (Show)
instance (FromJSON a) => FromJSON (Results a) where
  parseJSON (Object o) = Results <$> o .: "numFound"
                                 <*> o .: "start"
                                 <*> o .: "maxScore"
                                 <*> o .: "docs"
  parseJSON _ = mzero

data SolrResponse a = SolrResponse {
  _responseHeader :: ResponseHeader,
  _response       :: Results a
} deriving (Show)
instance (FromJSON a) => FromJSON (SolrResponse a) where
  parseJSON (Object o) = SolrResponse <$> o .: "responseHeader"
  			              <*> o .: "response"
  parseJSON _ = mzero

data DocHolder a = DocHolder {
  _userData :: a,
  _metadata :: Doc
} deriving (Show)
instance (FromJSON a) => FromJSON (DocHolder a) where
  parseJSON val = DocHolder <$> parseJSON val
                            <*> parseJSON val

makeLenses ''SolrResponse
makeLenses ''Results
makeLenses ''DocHolder
makeLenses ''Doc
makeLenses ''ResponseHeader

getDocHolders :: SolrResponse a -> [DocHolder a]
getDocHolders = view $ response . docs

--Testing stuff

-- Name is a temporary data type while the lib is being
-- constructed. It is as if we stored {"name":"Chris"}
-- in the database. The client should create a docHolder
-- with a Doc (static IDs and scores) and Name (actual
-- data stored)
data Name = Name {
  _name :: String
} deriving (Show)
instance FromJSON Name where
  parseJSON (Object o) = Name <$> o .: "name"

-- JSON to be parsed into a Params
paramsJSON :: L.ByteString
paramsJSON = "{\"shards\":\"127.0.0.1:8093/solr/my_index\",\"q\":\"*:*\",\"wt\":\"json\"}"
-- Attempt to parse above data
p :: Maybe Params
p = decode paramsJSON

-- JSON to be parsed into a ResponseHeader
rhJSON :: L.ByteString
rhJSON = "{\"status\":0,\"QTime\":11,\"params\":{\"shards\":\"127.0.0.1:8093/solr/my_index\",\"q\":\"*:*\",\"wt\":\"json\"}}"
-- Attempt to parse above data
rh :: Maybe ResponseHeader
rh = decode rhJSON

-- JSON to be parsed into a DocHolder
docHolderJSON :: L.ByteString
docHolderJSON = "{\"loc\":\"37.774929,-122.419416\",\"name\":\"San Francisco\",\"_yz_id\":\"geo_type_stuff_sf_60_5n4gT2r1Gt2qlHwfKCwypL\",\"_yz_rk\":\"sf\",\"_yz_rt\":\"geo_type\",\"_yz_rb\":\"stuff\",\"score\":0.39857662}"
-- Attempt to parse above data using
-- Name data type
dH :: Maybe (DocHolder Name)
dH = decode docHolderJSON
