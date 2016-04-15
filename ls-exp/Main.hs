{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Reflex
import Reflex.Dom
import Reflex.Dom.Widget
import Reflex.Dom.Xhr
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.HashMap.Strict as HMS

host = "localhost:2000"
sha = "104cc85b5be3d25795df638a9e83bed286da7070"
url = "http://localhost:2000/xhrtree/104cc85b5be3d25795df638a9e83bed286da7070"

-- from MissingH
startswith :: Eq a => [a] -> [a] -> Bool
startswith = L.isPrefixOf

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
    in firstline : case remainder of
      [] -> []
      x -> if x == delim
           then [] : []
           else split delim (drop (length delim) x)

-- TODO: refactor to the same sources!!
tpack :: String -> T.Text
tpack = T.pack

tap ∷ T.Text → T.Text → T.Text
tap = T.append

data DocTreePath = DocTreePath
                   { pRevisionHash :: T.Text
                     -- ^ a SHA1
                   , pFilePath :: [T.Text]
                   , pNodePath :: [Double]
                     -- ^ Child indices in trees.  The empty list is
                     -- the root.  The nth child is [n].
                   } deriving (Eq, Show)

instance ToJSON DocTreePath where
  toJSON (DocTreePath hsh path nodep) =
    String (hsh `tap` (T.intercalate "/" path) `tap` "?path=" `tap` (
               tpack $ L.intercalate "," $ map show nodep))

instance FromJSON DocTreePath where
  parseJSON (String t) =
    let s = T.unpack t
        (Just firstSlash) = L.elemIndex '/' s
        (Just qMark) = L.elemIndex '?' s
        (sha, rest1) = splitAt firstSlash s
        (path, rest2) = splitAt (qMark - firstSlash) rest1
        append (lst,cs) c = if c == ',' then (lst ++ [cs],[]) else (lst, cs ++ [c])
        (penultimate, last) = foldl append ([],[]) $ drop 6 rest2
        npath = map (read ∷ String → Double) $ penultimate ++ [last]
        -- must split path into individual segments.
    in return  (DocTreePath (T.pack sha) (map T.pack $ split "/" path) npath)

data DocTreeEntity = DTIssue String| DTOrg
                     deriving (Eq, Show)

instance FromJSON DocTreeEntity where
  parseJSON (Object obj) =
    if null obj
    then return DTOrg
    else let getKey ∷ T.Text → String
             getKey key =
               case (HMS.lookupDefault (String "unknown") key obj) of 
                 String s → T.unpack s
                 _ → "wrong type"
             origin = getKey "origin"
             number = getKey "number"
             user = getKey "user"
         in return $ DTIssue (origin ++ "/" ++ user ++ "/#" ++ number)

data DocTreeKind = TKDir | TKOrgNode | TKText | TKTable | TKSource (Maybe T.Text)
                 | TKEntity T.Text deriving (Eq, Show)

instance FromJSON DocTreeKind where
  parseJSON (String s) = return $ case s of
    "directory" → TKDir
    "org" → TKOrgNode
    "text" → TKText
    "table" → TKTable
    "src" → TKSource Nothing
    _ → if "src-" `T.isPrefixOf` s
        then TKSource (Just $ T.drop 4 s)
        else TKEntity s

data DocTreeEntry = DocTreeEntry
                    { tTitle ∷ T.Text
                    , tPath ∷ DocTreePath
                    , tChildren ∷ [DocTreeEntry]
                    , tKind :: DocTreeKind
                    , tTags :: [T.Text]
                    , tState :: Maybe T.Text
                    } deriving (Eq, Show)

instance FromJSON DocTreeEntry where
  parseJSON (Object ent) =
    DocTreeEntry <$>
    ent .: "title" <*>
    ent .: "path" <*>
    ent .: "children" <*>
    ent .: "kind" <*>
    ent .: "tags" <*>
    ent .: "state"

main = do
  -- something goes here
  mainWidget $ el "div" $ do
    -- use getPostBuild in Reflex.Dom.Class and then swap the () event with an xhrRequest.
    -- get the event that fires as soon as we're up.
    postBuildEvt ← getPostBuild
    -- And make a new one that creates an xhrRequest from that.
    reqEvt ← performRequestAsync $ ffor postBuildEvt (\_ → xhrRequest "GET" url def)
    let traceXhrFn res = "XHR: " ++ (T.unpack $ _xhrResponse_statusText res)
        tracedXhr = traceEventWith traceXhrFn reqEvt
        resultText xhr = case _xhrResponse_responseText xhr of
          Nothing → "Failed"
          Just s → show s
    textVal ← holdDyn "waiting" $ fmap resultText tracedXhr
    dynText $ textVal
