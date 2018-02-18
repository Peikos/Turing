{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where
import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Data.Text (Text)
import Data.Text.Lazy (unpack)

import Turing

import Happstack.Server
import Text.Parsec hiding (State(..))

import Text.Blaze.Html5 (Html, a, p, toHtml, (!), Html, (!), form, input, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

#if defined(linux_HOST_OS)
import qualified Data.ByteString.Char8 as B
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
instance ToMessage Html where
  toContentType _ = B.pack "text/html; charset=UTF-8"
  toMessage       = Blaze.renderHtml
#endif

main :: IO ()
main = do simpleHTTP nullConf { port = 9999 } $ decodeBody (defaultBodyPolicy "/tmp" 0 1000 1000) >> turingForm
turingForm :: ServerPart Response
turingForm = msum [ viewForm, processForm] where
  viewForm :: ServerPart Response
  viewForm = do method GET
                ok $ template "Turing" $ H.form ! action "/" ! enctype "multipart/form-data" ! A.method "POST" $ do
                    p "Machine: < initial state, [final state, final state, ...], [mapping, ...] >"
                    input ! A.id "machine" ! name "machine" ! value "<1, [E], [1 _ -> 1 _ L, 1 A -> 2 A L, 1 C -> 1 A R, 2 _ -> E A R, 2 A -> 2 B L]>"
                    input ! A.id "tape" ! name "tape" ! value "C C C"
                    p "Tape: Symbol Symbol ..."
                    H.br
                    input ! type_ "submit" ! value "Probeer!"

  processForm :: ServerPart Response
  processForm = do method POST
                   tm <- parseMachine "machine"
                   t <- parseTape "tape"
                   liftIO $ print t
                   liftIO $ print tm
                   let results = finalTape tm t
                   ok $ template "Result" $ do
                     H.p $ toHtml $ showResults tm t
                     H.p $ toHtml $ results

parseMachine :: String -> ServerPart Turing
parseMachine = fmap (fromRight (Turing [] (St "Q") [St "Q"]) . parse tm "" . unpack) . lookText

parseTape :: String -> ServerPart Tape
parseTape = fmap (fromRight (Tape [] []) . parse tape "" . unpack) . lookText

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head . H.title . toHtml $ title
    H.body $ do
      p . H.h1 . toHtml $ title
      body

fromRight :: b -> Either a b -> b
fromRight _ (Right a) = a
fromRight b (Left _)  = b

finalTape :: Turing -> Tape -> String
finalTape tm t = (\(Tape l r) -> map retrieveChar l ++ takeWhile (not . (== '_')) (map retrieveChar r)) . snd . last . rights . run tm $ t
