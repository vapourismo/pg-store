{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Test where

import qualified Data.ByteString as B
import           Database.PostgreSQL.Store.TH

data Movie = Movie {
	title :: B.ByteString,
	year  :: Int
} deriving Show

makeTable ''Movie
