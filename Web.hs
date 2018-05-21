{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverloadedStrings #-}

module Main where
import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Either (rights)
import Data.Text (Text)
import Data.Text.Lazy (unpack)

import Turing

import Happstack.Server
import Text.Parsec hiding (State(..))

import Text.Blaze.Html5 (Html, a, p, toHtml, (!), Html, (!), form, input, textarea, label, em)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- #if defined(linux_HOST_OS)
--import qualified Data.ByteString.Char8 as B
--import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
--instance ToMessage Html where
  --toContentType _ = B.pack "text/html; charset=UTF-8"
  --toMessage       = Blaze.renderHtml
-- #endif

main :: IO ()
main = do putStrLn "Starting server on port 9999 ..."
          simpleHTTP nullConf { port = 9999 } $ decodeBody (defaultBodyPolicy "/tmp" 0 1000 1000) >> turingForm

turingForm :: ServerPart Response
turingForm = msum [ viewForm, processForm] where
  viewForm :: ServerPart Response
  viewForm = do method GET
                ok $ template "Turing" $ H.form ! action "/" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    p $ "Initial state, e.g. " <> em "1"
                    input ! A.id "initial" ! name "initial" ! value "1"
                    p $ "Final states, e.g. " <> em "[A, B, C]"
                    input ! A.id "final" ! name "final" ! value "[E]"
                    p $ "Mapping, e.g. " <> em "[1 A -> 1 B R, ...]"
                    textarea "[1 _ -> 1 _ L,\n1 A -> 2 A L,\n1 C -> 1 A R,\n2 _ -> E A R,\n2 A -> 2 B L]" ! A.id "mapping" ! name "mapping" ! A.rows "6"
                    p $ "Tape, e.g. " <> em "Symbol Symbol ..."
                    input ! A.id "tape" ! name "tape" ! value "C C C"
                    H.br
                    input ! type_ "submit" ! value "Probeer!"

  processForm :: ServerPart Response
  processForm = do method POST
                   i <- parseInitial "initial"
                   fs <- parseFinals "final"
                   ms <- parseMappings "mapping"
                   t <- parseTape "tape"
                   liftIO $ print i
                   liftIO $ print fs
                   liftIO $ print ms
                   let tm = Turing ms i fs
                   liftIO $ print tm
                   let results = finalTape tm t
                   ok $ template "Result" $ do
                     H.p $ toHtml $ showResults tm t
                     H.p $ toHtml $ results

parseInput :: Parser a -> a -> String -> ServerPart a
parseInput p d = fmap (fromRight d . parse p "" . unpack) . lookText

parseInitial :: String -> ServerPart State
parseInitial = parseInput state (St "Q")

parseFinals :: String -> ServerPart [State]
parseFinals = parseInput states [St "Q"]

parseMappings :: String -> ServerPart [((State, Symbol), (State, Symbol, Direction))]
parseMappings = parseInput mappings []

parseTape :: String -> ServerPart Tape
parseTape = parseInput tape (Tape [] [])

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title . toHtml $ title
      H.style "body { font-family: sans-serif; }"
    H.body $ do
      p . H.h1 . toHtml $ title
      body

fromRight :: b -> Either a b -> b
fromRight _ (Right a) = a
fromRight b (Left _)  = b

finalTape :: Turing -> Tape -> String
finalTape tm t = (\(Tape l r) -> map retrieveChar l ++ takeWhile (not . (== '_')) (map retrieveChar r)) . snd . last . rights . run tm $ t
