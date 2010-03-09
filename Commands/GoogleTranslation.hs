module Commands.GoogleTranslation
    (
      getGoogleTranslation
    ) where

import Control.Arrow
import Control.Applicative
import Data.Char
import Data.Maybe (fromMaybe)
import Data.ByteString.Internal (c2w)
import Text.JSONb
import Network.Curl

import qualified Data.ByteString.UTF8       as B8
import qualified Data.Map                   as M
import qualified Data.Trie                  as T

import Utils

type Language = String

googleAjaxURL from to text = 
    "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=" ++ (urlEscape text') ++ "&langpair=" ++ (urlEscape from') ++ "%7C" ++ (urlEscape to')

  where text' = urlEscape text
        from' = urlEscape from
        to'   = urlEscape to

-- | Perform the CURL 
getGoogleTranslation :: Language -> Language -> String -> IO (Maybe (Either String String))
getGoogleTranslation from to text = do
    let

        from' = fromMaybe from $ languageByName from
        to'   = fromMaybe to   $ languageByName to

    c <- (second decode) <$> curlGetString_ (googleAjaxURL from' to' text) method_GET
    return $ case c of
                  (CurlOK, Right json) ->
                      case jsonToTranslation json of
                           Nothing     -> Just . Left $ "Couldn't translate: \"" ++ text ++ "\" from \"" ++ from ++ "\" to \"" ++ to ++ "\""
                           translation -> fmap Right translation
                  _                    -> Nothing

-- | Get the "translatedText" element out of our JSON object
jsonToTranslation :: JSON -> Maybe String
jsonToTranslation (Object m) = do
    Number status <- T.lookup (B8.fromString "responseStatus") m
    case status of
         200 -> do
             Object translated <- T.lookup (B8.fromString "responseData") m
             String text       <- T.lookup (B8.fromString "translatedText") translated
             return . concat . lines $ B8.toString text
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
