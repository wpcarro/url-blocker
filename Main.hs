{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Main ( main ) where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Aeson as Aeson
import qualified Data.Either.Combinators as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import qualified Data.List as List

import GHC.Generics
import Data.Aeson ((.:))
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype URL = URL { getURL :: Text } deriving (Show, Eq, Generic)

newtype IPAddress = IPAddress { getIPAddress :: Text } deriving (Show)

newtype Domain = Domain { getDomain :: Text } deriving (Show)

newtype Hour = Hour { getHour :: Int } deriving (Show, Eq, Generic)

newtype Minute = Minute { getMinute :: Int } deriving (Show, Eq, Generic)

data EtcHostsEntry = EtcHostsEntry { ip :: IPAddress
                                   , domains :: [Domain]
                                   } deriving (Show)

-- | Write these in terms of your system's local time (i.e. `date`).
data TimeSlot = TimeSlot { beg :: (Hour, Minute)
                         , end :: (Hour, Minute)
                         } deriving (Show, Eq, Generic)

data Allowance = Allowance { day :: Calendar.DayOfWeek
                           , timeslots :: [TimeSlot]
                           } deriving (Show, Eq, Generic)

data Rule = Rule { urls :: [URL]
                 , allowed :: [Allowance]
                 } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Aeson.FromJSON TimeSlot where
  parseJSON = Aeson.withText "timeslot" $ \x -> do
    let [a, b] = Text.splitOn "-" x
        [ah, am] = Text.splitOn ":" a
        [bh, bm] = Text.splitOn ":" b
    case extractTimeSlot ah am bh bm of
      Left s  -> fail s
      Right x -> pure x
    where
      extractTimeSlot :: Text -> Text -> Text -> Text -> Either String TimeSlot
      extractTimeSlot ah am bh bm = do
        (begh, _) <- TextRead.decimal ah
        (begm, _) <- TextRead.decimal am
        (endh, _) <- TextRead.decimal bh
        (endm, _) <- TextRead.decimal bm
        pure $ TimeSlot{ beg = (Hour begh, Minute begm)
                       , end = (Hour endh, Minute endm)
                       }

instance Aeson.FromJSON Allowance where
  parseJSON = Aeson.withObject "allowance" $ \x -> do
    day <- x .: "day"
    timeslots <- x .: "timeslots"
    pure $ Allowance{day, timeslots}

instance Aeson.FromJSON URL where
  parseJSON = Aeson.withText "URL" $ \x -> do
    pure $ URL { getURL = x }

instance Aeson.FromJSON Rule where
  parseJSON = Aeson.withObject "rule" $ \x -> do
    urls <- x .: "urls"
    allowed <- x .: "allowed"
    pure Rule{urls, allowed}

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Pipe operator
(|>) :: a -> (a -> b) -> b
(|>) a f = f a
infixl 1 |>

-- | Returns True if the current time falls within any of the `timeslots`.
isWithinTimeSlot :: LocalTime.LocalTime -> [TimeSlot] -> Bool
isWithinTimeSlot date timeslots =
  List.any withinTimeSlot timeslots
  where
    withinTimeSlot :: TimeSlot -> Bool
    withinTimeSlot TimeSlot{ beg = (Hour ah, Minute am)
                           , end = (Hour bh, Minute bm)
                           } =
      let LocalTime.TimeOfDay{LocalTime.todHour, LocalTime.todMin} =
            LocalTime.localTimeOfDay date
      in (todHour > ah) && (todMin > am) && (todHour < bh) && (todMin < bm)

-- | Returns True if `day` is the same day as today.
isToday :: LocalTime.LocalTime -> Calendar.DayOfWeek -> Bool
isToday date day = today == day
  where
    today = Calendar.dayOfWeek (LocalTime.localDay date)

-- | Returns True if a list of none of the `allowances` are valid.
shouldBeBlocked :: LocalTime.LocalTime -> [Allowance] -> Bool
shouldBeBlocked _ [] = True
shouldBeBlocked date allowances = do
  case filter (isToday date . day) allowances of
    [Allowance{timeslots}] -> not $ isWithinTimeSlot date timeslots
    [] -> True
    -- Error when more than one rule per day
    _  -> True

-- | Maps an EtcHostsEntry to the line of text url-blocker will append to /etc/hosts.
serializeEtcHostEntry :: EtcHostsEntry -> Text
serializeEtcHostEntry EtcHostsEntry{ip, domains} =
  (getIPAddress ip) <> "\t" <> (Text.unwords $ fmap getDomain domains)

-- | Create an EtcHostsEntry mapping the URLs in `rule` to 127.0.0.1 if the
-- URLs should be blocked.
maybeBlockURL :: LocalTime.LocalTime -> Rule -> Maybe EtcHostsEntry
maybeBlockURL date Rule{urls, allowed} =
  if shouldBeBlocked date allowed then
    Just $ EtcHostsEntry { ip = IPAddress "127.0.0.1"
                        , domains = fmap (Domain . getURL) urls
                        }
  else
    Nothing

-- | Read and parse the rules.json file.
-- TODO(wpcarro): Properly handle errors for file not found.
-- TODO(wpcarro): Properly handle errors for parse failures.
-- TODO(wpcarro): How can we resolve the $HOME directory when this is run as
-- root?
getRules :: IO [Rule]
getRules = do
  contents <- LazyByteString.readFile "/home/wpcarro/.config/url-blocker/rules.json"
  let payload = Aeson.eitherDecode contents
  pure $ Either.fromRight [] payload

-- | Informational header added to /etc/hosts before the entries that
-- url-blocker adds.
urlBlockerHeader :: Text
urlBlockerHeader =
  Text.unlines [ "################################################################################"
               , "# Added by url-blocker."
               , "#"
               , "# Warning: url-blocker will remove anything that you add beneath this header."
               , "################################################################################"
               ]

-- | Removes all entries that url-blocker may have added to /etc/hosts.
removeURLBlockerEntries :: Text -> Text
removeURLBlockerEntries etcHosts =
  case Text.breakOn urlBlockerHeader etcHosts of
    (etcHosts', _) -> etcHosts'

-- | Appends the newly created `entries` to `etcHosts`.
addURLBlockerEntries :: Text -> Text -> Text
addURLBlockerEntries entries etcHosts =
  Text.unlines [ etcHosts
               , urlBlockerHeader
               , entries
               ]

-- | This script reads the current /etc/hosts, removes any entries that
-- url-blocker may have added in a previous run, and adds new entries to block
-- URLs according to the rules.json file.
main :: IO ()
main = do
  rules <- getRules
  tz <- LocalTime.getCurrentTimeZone
  ct <- Clock.getCurrentTime
  let date = LocalTime.utcToLocalTime tz ct
      entries = rules
                |> fmap (maybeBlockURL date)
                |> Maybe.catMaybes
                |> fmap serializeEtcHostEntry
                |> Text.unlines
  existingEtcHosts <- TextIO.readFile "/etc/hosts"
  existingEtcHosts
    |> removeURLBlockerEntries
    |> addURLBlockerEntries entries
    |> \x -> writeFile "/etc/hosts" (Text.unpack x)
