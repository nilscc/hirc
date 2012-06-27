{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module Modules.GoogleTranslate
  ( googleTranslate
  , getGoogleTranslation
  ) where

import Prelude hiding (concat, lines)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Attoparsec
import Data.Maybe
import Network.Curl
import Network.URL

import qualified Data.ByteString.Char8  as BS
import qualified Data.Map               as M

import Hirc

--------------------------------------------------------------------------------
-- The main module

googleTranslate :: Module
googleTranslate = Module "Google Translate" $
  onValidPrefix $
    userCommand $ \("translate" :: String) lang1 lang2 (unwords -> what) -> do
      result <- getGoogleTranslation lang1 lang2 what
      case result of
           Just (Right translation) -> withNickname $ \n -> do
             logM 2 $ "Sending translation to " ++ show n
             answer translation
           e ->
             logM 2 $ "Translation failed with: " ++ show e 
      done

--------------------------------------------------------------------------------
-- Types & stuff

-- Data types & Aeson instance

newtype GoogleResult = GoogleResult [String]

instance FromJSON GoogleResult where
  parseJSON (Object m) = do
    n  <- m .: "data"
    ts <- n .: "translations"
    GoogleResult <$> forM ts (.: "translatedText")
  parseJSON _ =
    mzero

type Language = String

-- HTTP settings

key :: String
key = "AIzaSyBuiFZJ07KBRvQBrntMJoGsHg7rddie5cU"

apiUrl :: Language -> Language -> String -> String
apiUrl source target q = exportURL $
  let Just url = importURL "https://www.googleapis.com/language/translate/v2"
      params   = [ ("key", key)
                 , ("source", source)
                 , ("target", target)
                 , ("q", q)
                 ]
   in foldr (flip add_param) url params

-- IO

getGoogleTranslation :: MonadIO m
                     => Language
                     -> Language
                     -> String
                     -> m (Maybe (Either String String))
getGoogleTranslation source target text = liftIO $ do
  let source' = fromMaybe source $ languageByName source
      target' = fromMaybe target $ languageByName target
  (code, rsp) <- curlGetString (apiUrl source' target' text) [CurlMaxFileSize (1000*1000)]
  case (code, parse json (BS.pack rsp)) of
       (CurlOK, Done _ j) ->
         case fromJSON j of
              Error _ ->
                return Nothing
              Success (GoogleResult []) ->
                return . Just . Left  $
                  "No translations for \"" ++ text ++ "\""
              Success (GoogleResult [t]) ->
                return . Just . Right $
                  "Translation: " ++ t
              Success (GoogleResult (t:ts)) ->
                return . Just . Right $
                  "Translation: " ++ t ++ " (" ++ show (length ts) ++ " more translations)"
       _ -> return Nothing


-- | Get a language ID by their full name
languageByName :: String -> Maybe String
languageByName = M.lookup `flip` languages

-- | Map of all available languages with their IDs
languages :: M.Map String String
languages = M.fromList $

  [ ("afrikaans", "af")
  , ("albanian", "sq")
  , ("amharic", "am")
  , ("arabic", "ar")
  , ("armenian", "hy")
  , ("azerbaijani", "az")
  , ("basque", "eu")
  , ("belarusian", "be")
  , ("bengali", "bn")
  , ("bihari", "bh")
  , ("bulgarian", "bg")
  , ("burmese", "my")
  , ("catalan", "ca")
  , ("cherokee", "chr")
  , ("chinese", "zh")
  , ("chinese_simplified", "zh-cn")
  , ("chinese_traditional", "zh-tw")
  , ("croatian", "hr")
  , ("czech", "cs")
  , ("danish", "da")
  , ("dhivehi", "dv")
  , ("dutch","nl")
  , ("english", "en")
  , ("esperanto", "eo")
  , ("estonian", "et")
  , ("filipino", "tl")
  , ("finnish", "fi")
  , ("french", "fr")
  , ("galician", "gl")
  , ("georgian", "ka")
  , ("german", "de")
  , ("greek", "el")
  , ("guarani", "gn")
  , ("gujarati", "gu")
  , ("hebrew", "iw")
  , ("hindi", "hi")
  , ("hungarian", "hu")
  , ("icelandic", "is")
  , ("indonesian", "id")
  , ("inuktitut", "iu")
  , ("irish", "ga")
  , ("italian", "it")
  , ("japanese", "ja")
  , ("kannada", "kn")
  , ("kazakh", "kk")
  , ("khmer", "km")
  , ("korean", "ko")
  , ("kurdish","ku")
  , ("kyrgyz","ky")
  , ("laothian","lo")
  , ("latvian", "lv")
  , ("lithuanian", "lt")
  , ("macedonian", "mk")
  , ("malay", "ms")
  , ("malayalam", "ml")
  , ("maltese", "mt")
  , ("marathi", "mr")
  , ("mongolian", "mn")
  , ("nepali", "ne")
  , ("norwegian", "no")
  , ("oriya", "or")
  , ("pashto", "ps")
  , ("persian", "fa")
  , ("polish", "pl")
  , ("portuguese", "pt-pt")
  , ("punjabi", "pa")
  , ("romanian", "ro")
  , ("russian", "ru")
  , ("sanskrit", "sa")
  , ("serbian", "sr")
  , ("sindhi", "sd")
  , ("sinhalese", "si")
  , ("slovak", "sk")
  , ("slovenian", "sl")
  , ("spanish", "es")
  , ("swahili", "sw")
  , ("swedish", "sv")
  , ("tajik", "tg")
  , ("tamil", "ta")
  , ("tagalog", "tl")
  , ("telugu", "te")
  , ("thai", "th")
  , ("tibetan", "bo")
  , ("turkish", "tr")
  , ("ukrainian", "uk")
  , ("urdu", "ur")
  , ("uzbek", "uz")
  , ("uighur", "ug")
  , ("vietnamese", "vi")
  , ("welsh", "cy")
  , ("yiddish", "yi")
  ]
