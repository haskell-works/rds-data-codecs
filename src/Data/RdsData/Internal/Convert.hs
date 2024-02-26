{-# LANGUAGE OverloadedStrings #-}

module Data.RdsData.Internal.Convert
  ( dayToText
  , jsonToText
  , timeOfDayToText
  , uuidToText
  , utcTimeToText
  , textToDouble
  ) where

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time
import Prelude hiding (maybe, null)

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.UUID            as UUID

timeOfDayToText :: TimeOfDay -> Text
timeOfDayToText = 
  T.pack . formatTime defaultTimeLocale "%H:%M:%S%Q"

dayToText :: Day -> Text
dayToText = 
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

jsonToText :: J.Value -> Text
jsonToText =
  T.decodeUtf8 . LBS.toStrict . J.encode

utcTimeToText :: UTCTime -> Text
utcTimeToText = 
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

uuidToText :: UUID -> Text
uuidToText = 
  T.pack . UUID.toString

textToDouble :: Text -> Maybe Double
textToDouble text =
  case reads (T.unpack text) of
    [(x, "")] -> Just x
    _         -> Nothing
