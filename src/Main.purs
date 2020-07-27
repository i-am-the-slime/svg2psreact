module Main where

import Prelude

import Data.Array (filter, last)
import Data.Either (Either(..))
import Data.Foldable (intercalate, null)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.Extra (camelCase)
import Effect (Effect)
import Effect.Class.Console (error)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Sync
import Node.Process (argv)
import Svg.Parser (Element, SvgAttribute(..), SvgNode(..), parseToSvgNode)

main :: Effect Unit
main = do
  arguments <- argv
  case last arguments of
    Just fileName -> do
      s <- Sync.readTextFile UTF8 fileName
      case parseToSvgNode s of
        Left e -> log (show e)
        Right g -> log $ renderNode 4 g
    Nothing -> error $ "Usage: node index.js pathToSvgFile"


renderNode :: Int -> SvgNode -> String
renderNode depth = case _ of
  SvgElement el -> renderElement depth el
  SvgText el -> el
  SvgComment comment -> comment

repeatS :: Int -> String -> String
repeatS n s =
  if n == 0 then s else s <> repeatS (n-1) s

renderElement :: Int -> Element -> String
renderElement depth { name, attributes, children } =
    name' <>
    brkSpc <> "{ " <> attributes' <> children' <>
    brkSpc <> "}"
   where
    spc = repeatS depth " "
    brkSpc = "\n" <> spc
    attrs = renderAttribute <$> attributes
    kids = renderNode (depth + 2) <$> children
    name' = "x = SVG." <> name
    attributes' = intercalate (brkSpc <> ", ") attrs
    commaBeforeChildren =
      if null attrs || null children
      then ""
      else (brkSpc <> ", ")
    children' = commaBeforeChildren <>
      if null children
      then ""
      else ("children: " <> brkSpc <> "[ " <>
        intercalate (brkSpc <> ", ") kids
        <> brkSpc <> "]")

quoted :: String -> String
quoted x = "\"" <> x <> "\""

renderAttribute :: SvgAttribute -> String
renderAttribute (SvgAttribute name value) = case name of
  "style" ->
    camelCase name <> ": " <> renderStyle value
  _ ->
    camelCase name <> ": " <> quoted value

renderStyle :: String -> String
renderStyle style = "css { " <> styleAttrs <> " }"
  where
    styles = filter (eq " " || eq "\n") $ split (Pattern ";") style
    toAttr x = case nameValue of
      [name, value] -> camelCase (trim name) <> ": " <> quoted value
      _ -> "\n -- ignored style: " <> "'" <> x <> "'" <> "\n"
      where
        nameValue = split (Pattern ":") x
    styleAttrs = intercalate ", " (toAttr <$> styles)
