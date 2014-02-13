{-# LANGUAGE OverloadedStrings #-}

module Types (SolrResponse) where

import Data.Aeson
import Data.Map
import Control.Applicative
import Control.Monad (mzero)

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)

paramsJSON :: L.ByteString
paramsJSON = "{\"shards\":\"127.0.0.1:8093/solr/my_index\",\"q\":\"*:*\",\"wt\":\"json\"}"
p :: Maybe Params
p = decode paramsJSON

rhJSON :: L.ByteString
rhJSON = "{\"status\":0,\"QTime\":11,\"params\":{\"shards\":\"127.0.0.1:8093/solr/my_index\",\"q\":\"*:*\",\"wt\":\"json\"}}"
rh :: Maybe ResponseHeader
rh = decode rhJSON

newtype Params = Params (Map String String) deriving (Show)
instance FromJSON Params where
  parseJSON val = Params <$> parseJSON val

data ResponseHeader = ResponseHeader {
  status :: Int,
  qTime  :: Int,
  params :: Params
} deriving (Show)
instance FromJSON ResponseHeader where
  parseJSON (Object o) = ResponseHeader <$> o .: "status"
									    <*> o .: "QTime"
										<*> o .: "params"
  parseJSON _ = mzero

data Docs = Docs {
  _yz_id :: String,
  _yz_rk :: String,
  _yz_rt :: String,
  _yz_rb :: String
} deriving (Show)
instance FromJSON Docs where
  parseJSON (Object o) = Docs <$> o .: "_yz_id"
                              <*> o .: "_yz_rk"
                              <*> o .: "_yz_rt"
                              <*> o .: "_yz_rb"
  parseJSON _ = mzero   

data Results = Results {
  numFound :: Int,
  start :: Int,
  maxScore :: Float,
  docs :: [Docs]
} deriving (Show)
instance FromJSON Results where
  parseJSON (Object o) = Results <$> o .: "numFound"
							     <*> o .: "start"
								 <*> o .: "maxScore"
								 <*> o .: "docs"
  parseJSON _ = mzero

data SolrResponse = SolrResponse {
  responseHeader :: ResponseHeader,
  response :: Results
} deriving (Show)
instance FromJSON SolrResponse where
  parseJSON (Object o) = SolrResponse <$> o .: "responseHeader"
  									  <*> o .: "response"
  parseJSON _ = mzero
  
