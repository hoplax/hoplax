#!/usr/bin/env runhaskell

-- Copyright 2011 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- Author: errge@google.com

-- Usage: import-bookmarks.hs < bookmarks.html
-- Usage: import-bookmarks.hs < bookmarks.json

-- It works with chrome/firefox html bookmark exports and firefox json
-- bookmark backup.  With Firefox you should stick with the json
-- backup, because this is the only way to keep your tags.

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import System (getArgs)
import Text.HTML.TagSoup
import Control.Monad
import Data.Function
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import Text.HJson
import Text.HJson.Query hiding (when)
import Debug.Trace
import Text.HJson.Pretty
import qualified Data.Map as Map
import MonadLib

strftime = formatTime defaultTimeLocale
strptime = readTime defaultTimeLocale
unixToUTC :: String -> UTCTime
unixToUTC = strptime "%s"
utcFormat :: UTCTime -> String
utcFormat = strftime "%Y-%m-%d %H:%M:%S"

data Bookmark = Bookmark { bUrl :: String,
                           bName :: String, -- aka title in ff json
                           bDescription :: Maybe String, -- bookmarkProperties/description in ff json
                           bShortcut :: Maybe String,
                           bDate :: Maybe UTCTime,
                           bTags :: [String],
                           bPath :: String
                         } deriving Show

-- Equality and sort is based on url only.
instance Eq Bookmark where
    (==) = (==) `on` bUrl

instance Ord Bookmark where
    compare = compare `on` bUrl

blacklist bmark =
    do let url = bUrl bmark
       guard $ not ("http://www.mozilla.com/en-US/firefox" `isPrefixOf` url)
       guard $ not ("place:" `isPrefixOf` url)
       guard $ not ("http://www.ubuntulinux.org/" `isPrefixOf` url)
       guard $ not ("https://addons.mozilla.org/en-US/firefox/bookmarks" `isPrefixOf` url)
       guard $ not ("http://www.debian.org/" `isPrefixOf` url)
       guard $ not ("https://answers.launchpad.net/ubuntu/+addquestion" `isPrefixOf` url)
       return bmark

------------------------------------------- bookmarks.html
-- state: current path (bookmarks can be in folders recursively)
-- choice: because some <DT>'s are not bookmarks

handleDT :: StateM m [String] => [Tag String] -> ChoiceT m Bookmark
handleDT soup =
    case soup !! 0 of
      TagOpen "DT" _ -> case soup !! 1 of
                          TagOpen "A" _ -> handleDTBookmark soup
                          TagOpen "H3" _ -> handleDTFolder soup
                          _ -> mzero
      TagClose "DL" -> do sets_ tail
                          mzero

handleDTFolder :: StateM m [String] => [Tag String] -> ChoiceT m Bookmark
handleDTFolder soup =
    let TagText fname = soup !! 2
    in do sets_ (fname:)
          mzero

mc :: Monad m => Maybe a -> ChoiceT m a
mc Nothing = mzero
mc (Just x) = return x

mlookup k l = mc $ lookup k l

delTrailingWS = reverse . dropWhile isSpace . reverse

handleDTBookmark :: StateM m [String] => [Tag [Char]] -> ChoiceT m Bookmark
handleDTBookmark soup =
    let TagOpen "A" attribs = soup !! 1
        TagText text = soup !! 2
    in do url <- mlookup "HREF" attribs
          let date = (mplus `on` (`lookup` attribs)) "LAST_MODIFIED" "ADD_DATE"
          let description = do guard $ length soup >= 7
                               TagOpen "DD" [] <- return $ soup !! 5
                               TagText text <- return $ soup !! 6
                               return $ delTrailingWS text
          path <- fmap (intercalate " -> " . reverse) get
          mc $ blacklist Bookmark {
                   bName = text,
                   bDescription = description,
                   bUrl = url,
                   bShortcut = lookup "SHORTCUTURL" attribs,
                   bDate = fmap unixToUTC date,
                   bTags = [],
                   bPath = path
                 }

mapMaybeM f (x:xs) = do mb <- findOne $ f x
                        put $ maybeToList mb
                        mapMaybeM f xs
mapMaybeM _ [] = return ()

-- mapMaybeM s f (x:xs) = case runStateT s (f x) of
--                          Just (Nothing, s') -> undefined -- mapMaybeM s' f xs

handleBookmarks str =
    let dts = partitions (\x -> x ~== "<DT>" || x ~== "</DL>") $ parseTags str
    in sort $ snd $ runId $ runWriterT $ runStateT [] $ mapMaybeM handleDT dts

------------------------------------------- bookmarks.json
-- Our beloved firefox (from version 3.5 (including) -> 4.0
-- (excluding)) puts a trailing comma at the end of bookmarks list.
-- Extra fun is, that Firefox >= 4 _rejects_ this in the JSON parser.
-- This is invalid JSON format and the hjson guys has nothing better
-- to do than validating with ultimate care.  So we fixup the stuff,
-- before calling hjson.
fixupFirefoxJSON json =
    let rjson = reverse json
    in case elemIndex '"' rjson of
      Nothing -> json
      Just i -> reverse $ filter (not . (== ',')) (take i rjson) ++ drop i rjson

-- walks a ff bookmark tree and returns a list of (url, tag) pairs and
-- a list of bookmarks
traverseJSON :: [String] -> Json -> [Either (String, [Char]) Bookmark]
traverseJSON path jobj@(JObject obj) =
    let attr x = Map.lookup x obj
        Just (JString title) = attr "title"
        cur = if attr "type" /= Just (JString "text/x-moz-place")
              then []
              else let Just (JString url) = attr "uri"
                   in if path !! 1 == "Tags"
                      then [Left (url, head path)]
                      else let shortcut = fmap (\(JString x) -> x) (attr "keyword")
                               date = fmap
                                      (\(JNumber x) -> unixToUTC $ show $ floor $ fromRational $ x / 10^6)
                                      ((mplus `on` attr) "lastModified" "dateAdded")
                               path' = intercalate " -> " $ reverse $ filter (/= "") path
                           in [Right Bookmark {
                                           bName = title,
                                           bUrl = url,
                                           bDescription =
                                               fmap (\(JString x) -> delTrailingWS x) $
                                               listToMaybe $
                                                               (getFromKeys ["annos"] >>>
                                                                getFromArr >>>
                                                                guards (getFromKeys ["name"] >>>
                                                                        isStrBy (=="bookmarkProperties/description"))
                                                                           (getFromKeys ["value"]))
                                                          jobj,
                                           bShortcut = shortcut,
                                           bDate = date,
                                           bPath = path',
                                           bTags = []
                                         }]
        children = case attr "children" of
                     Just (JArray cs) -> concatMap (traverseJSON $ title:path) cs
                     Nothing -> []
                     Just _ -> error "children is not an array"
    in cur ++ children
traverseJSON _ _ = undefined

handleJSON str = either
                 (error . show)
                 (partitionEithers . traverseJSON [])
                 (fromString $ fixupFirefoxJSON str)

groupFst :: Eq a => [(a, b)] -> [(a, [b])]
groupFst l = let groups = groupBy ((==) `on` fst) l
             in map (\g -> (fst $ head g, map snd g)) groups

merge :: Ord a => (Maybe b1 -> Maybe b2 -> Maybe c) -> [(a, b1)] -> [(a, b2)] -> [c]
merge f xss@((ix,x):xs) yss@((iy,y):ys) | ix < iy = maybeToList (f (Just x) Nothing) ++ merge f xs yss
                                        | ix > iy = maybeToList (f Nothing (Just y)) ++ merge f xss ys
                                        | ix == iy = maybeToList (f (Just x) (Just y)) ++ merge f xs ys
merge f ((_,x):xs) [] = maybeToList (f (Just x) Nothing) ++ merge f xs []
merge f [] ((_,y):ys) = maybeToList (f Nothing (Just y)) ++ merge f [] ys
merge _ [] [] = []

tagMerge :: Maybe [String] -> Maybe [Bookmark] -> Maybe [Bookmark]
tagMerge _ Nothing = Nothing
tagMerge Nothing bookmarks = bookmarks
tagMerge (Just tags) (Just bmarks) = Just $ map addTags bmarks
    where
      addTags bmark = bmark { bTags = [] } -- tags }

jEscape = Text.HJson.toString . JString

jMEscape prefix = maybe "" ((prefix ++) . Text.HJson.toString . JString)

jLEscape = Text.HJson.toString . JArray . map JString

printBMark b =
    putStr $ "  { name: " ++ (jEscape $ bName b) ++
             ", url: " ++ (jEscape $ bUrl b) ++
             ", tags: " ++ (jLEscape $ bTags b) ++
             ", path: " ++ (jEscape $ bPath b) ++
             jMEscape ", keyword: " (bShortcut b) ++
             jMEscape ", date: " (fmap utcFormat $ bDate b) ++
             jMEscape ", description: " (bDescription b) ++ " }"

main = do str <- getContents
          let taggedBookmarks =
                  case head str of
                    '{' -> let (tagmap, bookmarks) = handleJSON str
                               filtBookmarks = groupFst $ map (\x -> (bUrl x, x)) $ sort $ bookmarks >>= blacklist
                               urlTags = groupFst $ sort tagmap
                           in concat $ merge tagMerge urlTags filtBookmarks
                    _ -> handleBookmarks str
          putStrLn "hoplax.bookmarks.push("
          sequence $ intersperse (putStrLn ",") $ map printBMark taggedBookmarks
          putStrLn "\n);"
