module Commands.GoogleTranslation
    (
      getGoogleTranslation
    ) where

import Control.Arrow
import Control.Applicative
import Data.Char
import Data.Maybe (fromMaybe)
import Data.ByteString.Internal (c2w)
import Text.HJson
import Network.Curl

import qualified Data.Map as M

import Utils

type Language = String

googleAjaxURL from to text = 
    "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=" ++ text' ++ "&langpair=" ++ from' ++ "%7C" ++ to'

  where text' = urlEscape text
        from' = urlEscape from
        to'   = urlEscape to

-- | Perform the CURL 
getGoogleTranslation :: Language -> Language -> String -> IO (Maybe (Either String String))
getGoogleTranslation from to text = do
    let

        from' = fromMaybe from $ languageByName from
        to'   = fromMaybe to   $ languageByName to

    c <- (second fromString) <$> curlGetString (googleAjaxURL from' to' text) method_GET
    return $ case c of
                  (CurlOK, Right json) ->
                      case jsonToTranslation json of
                           Nothing     -> Just . Left $ "Couldn't translate: \"" ++ text ++ "\" from \"" ++ from ++ "\" to \"" ++ to ++ "\""
                           translation -> fmap Right translation
                  _                    -> Nothing

-- | Get the "translatedText" element out of our JSON object
jsonToTranslation :: Json -> Maybe String
jsonToTranslation (JObject m) = do
    JNumber status <- M.lookup "responseStatus" m
    case status of
         200 -> do
             JObject translated <- M.lookup "responseData" m
             JString text       <- M.lookup "translatedText" translated
             return . concat $ lines text
         _ ->
             Nothing

jsonToTranslation _ = Nothing

-- | Get a language ID by their full name
languageByName :: String -> Maybe String
languageByName = M.lookup `flip` languages

-- | Map of all available languages with their IDs
languages :: M.Map String String
languages = M.fromList . map (first (map toLower)) $

  [ ("AFRIKAANS", "af")
  , ("ALBANIAN", "sq")
  , ("AMHARIC", "am")
  , ("ARABIC", "ar")
  , ("ARMENIAN", "hy")
  , ("AZERBAIJANI", "az")
  , ("BASQUE", "eu")
  , ("BELARUSIAN", "be")
  , ("BENGALI", "bn")
  , ("BIHARI", "bh")
  , ("BULGARIAN", "bg")
  , ("BURMESE", "my")
  , ("CATALAN", "ca")
  , ("CHEROKEE", "chr")
  , ("CHINESE", "zh")
  , ("CHINESE_SIMPLIFIED", "zh-CN")
  , ("CHINESE_TRADITIONAL", "zh-TW")
  , ("CROATIAN", "hr")
  , ("CZECH", "cs")
  , ("DANISH", "da")
  , ("DHIVEHI", "dv")
  , ("DUTCH","nl")
  , ("ENGLISH", "en")
  , ("ESPERANTO", "eo")
  , ("ESTONIAN", "et")
  , ("FILIPINO", "tl")
  , ("FINNISH", "fi")
  , ("FRENCH", "fr")
  , ("GALICIAN", "gl")
  , ("GEORGIAN", "ka")
  , ("GERMAN", "de")
  , ("GREEK", "el")
  , ("GUARANI", "gn")
  , ("GUJARATI", "gu")
  , ("HEBREW", "iw")
  , ("HINDI", "hi")
  , ("HUNGARIAN", "hu")
  , ("ICELANDIC", "is")
  , ("INDONESIAN", "id")
  , ("INUKTITUT", "iu")
  , ("IRISH", "ga")
  , ("ITALIAN", "it")
  , ("JAPANESE", "ja")
  , ("KANNADA", "kn")
  , ("KAZAKH", "kk")
  , ("KHMER", "km")
  , ("KOREAN", "ko")
  , ("KURDISH","ku")
  , ("KYRGYZ","ky")
  , ("LAOTHIAN","lo")
  , ("LATVIAN", "lv")
  , ("LITHUANIAN", "lt")
  , ("MACEDONIAN", "mk")
  , ("MALAY", "ms")
  , ("MALAYALAM", "ml")
  , ("MALTESE", "mt")
  , ("MARATHI", "mr")
  , ("MONGOLIAN", "mn")
  , ("NEPALI", "ne")
  , ("NORWEGIAN", "no")
  , ("ORIYA", "or")
  , ("PASHTO", "ps")
  , ("PERSIAN", "fa")
  , ("POLISH", "pl")
  , ("PORTUGUESE", "pt-PT")
  , ("PUNJABI", "pa")
  , ("ROMANIAN", "ro")
  , ("RUSSIAN", "ru")
  , ("SANSKRIT", "sa")
  , ("SERBIAN", "sr")
  , ("SINDHI", "sd")
  , ("SINHALESE", "si")
  , ("SLOVAK", "sk")
  , ("SLOVENIAN", "sl")
  , ("SPANISH", "es")
  , ("SWAHILI", "sw")
  , ("SWEDISH", "sv")
  , ("TAJIK", "tg")
  , ("TAMIL", "ta")
  , ("TAGALOG", "tl")
  , ("TELUGU", "te")
  , ("THAI", "th")
  , ("TIBETAN", "bo")
  , ("TURKISH", "tr")
  , ("UKRAINIAN", "uk")
  , ("URDU", "ur")
  , ("UZBEK", "uz")
  , ("UIGHUR", "ug")
  , ("VIETNAMESE", "vi")
  , ("WELSH", "cy")
  , ("YIDDISH", "yi")
  ]
