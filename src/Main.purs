module Main where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Array (filter, last, notElem)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate, null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.String as S
import Data.String.Extra (camelCase, pascalCase)
import Data.String.Utils as SU
import Effect (Effect)
import Effect.Aff (launchAff_, throwError)
import Effect.Aff as Aff
import Effect.Console (log, error)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as Async
import Node.FS.Sync as Sync
import Node.Path as Path
import Node.Process (argv)
import Svg.Parser (Element, SvgAttribute(..), SvgNode(..), parseToSvgNode)
import Text.Parsing.StringParser (ParseError)

main ∷ Effect Unit
main = do
  arguments <- argv
  case last arguments of
    Just fileName -> do
      s <- Sync.readTextFile UTF8 fileName
      case parse "Assets" fileName s of
        Left e -> log (show e)
        Right result -> log result
    Nothing -> error $ "Usage: node index.js pathToSvgFile"

convertAllSVGsInDirectory :: EffectFn2 String String Unit
convertAllSVGsInDirectory = mkEffectFn2 processAllInDirectory

processAllInDirectory :: String -> String -> Effect Unit
processAllInDirectory modulePrefix dir =
  launchAff_ do
    filePaths <- Async.readdir dir
    parTraverse_ (processFile filePaths) filePaths
  where
  processFile filePaths filePath =
    when (isSVGFile && notAlreadyWritten) do
      svgString <- Async.readTextFile UTF8 svgFilePath
      case parse modulePrefix svgFilePath svgString of
        Left parseError -> throwError (Aff.error $ "Parse error on " <> svgFilePath <> "\r\nError: " <> show parseError)
        Right pureScriptCode -> Async.writeTextFile UTF8 pursFilePath pureScriptCode
    where
    isSVGFile = (SU.endsWith ".svg" || SU.endsWith ".SVG") filePath
    svgFilePath = Path.concat [dir, filePath]
    pursFilePath = Path.concat [dir, purescriptName]
    notAlreadyWritten = filePaths # notElem purescriptName
    nameWithoutSuffix = (stripSuffixOrNot (S.Pattern ".svg") >>> stripSuffixOrNot (S.Pattern ".SVG")) filePath
    purescriptName = pascalCase nameWithoutSuffix <> ".purs"

parse ∷ String -> String -> String -> Either ParseError String
parse modulePrefix fileName s = parseToSvgNode s <#> \g -> header modulePrefix fileName <> renderNode 4 g

header ∷ String -> String -> String
header modulePrefix fileName =
  "module " <> moduleName
    <> """ where

import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.SVG as SVG

"""
    <> varName
    <> " :: JSX\n"
    <> varName
    <> " = "
  where
  sanitisedFileName = sanitiseFileName fileName
  moduleName = modulePrefix <> "." <> pascalCase sanitisedFileName
  varName = camelCase sanitisedFileName

sanitiseFileName ∷ String -> String
sanitiseFileName =
  S.replaceAll (S.Pattern "\\") (S.Replacement "/")
    <<< getLastSplit (S.Pattern "/")
    <<< stripSuffixOrNot (S.Pattern ".svg")
    <<< stripSuffixOrNot (S.Pattern ".SVG")
    <<< S.trim
    <<< S.replaceAll (S.Pattern " ") (S.Replacement "_")

stripSuffixOrNot ∷ S.Pattern -> String -> String
stripSuffixOrNot suffix s = fromMaybe s $ S.stripSuffix suffix s

getLastSplit ∷ Pattern -> String -> String
getLastSplit pattern s = fromMaybe s $ Array.last $ S.split pattern s

renderNode ∷ Int -> SvgNode -> String
renderNode depth = case _ of
  SvgElement el -> renderElement depth el
  SvgText el -> "R.text " <> quoted el
  SvgComment comment -> comment

repeatS ∷ Int -> String -> String
repeatS n s = if n == 0 then s else s <> repeatS (n - 1) s

renderElement ∷ Int -> Element -> String
renderElement depth { name, attributes, children } =
  name'
    <> brkSpc
    <> "{ "
    <> attributes'
    <> children'
    <> brkSpc
    <> "}"
  where
  spc = repeatS depth " "
  brkSpc = "\n" <> spc
  attrs = renderAttribute <$> attributes
  kids = renderNode (depth + 2) <$> children
  name' = "SVG." <> name
  attributes' = intercalate (brkSpc <> ", ") attrs
  commaBeforeChildren =
    if null attrs || null children then
      ""
    else
      (brkSpc <> ", ")
  children' =
    commaBeforeChildren
      <> if null children then
          ""
        else
          "children: " <> brkSpc <> "[ "
            <> intercalate (brkSpc <> ", ") kids
            <> brkSpc
            <> "]"

quoted ∷ String -> String
quoted x = "\"" <> x <> "\""

renderAttribute ∷ SvgAttribute -> String
renderAttribute (SvgAttribute name value) = case name of
  "style" -> camelCase name <> ": " <> renderStyle value
  "class" -> "className:" <> quoted value
  _ -> camelCase name <> ": " <> quoted value

renderStyle ∷ String -> String
renderStyle style = "R.css { " <> styleAttrs <> " }"
  where
  styles = filter (eq " " || eq "\n") $ split (Pattern ";") style
  toAttr x = case nameValue of
    [ name, value ] -> camelCase (trim name) <> ": " <> quoted value
    _ -> "\n -- ignored style: " <> "'" <> x <> "'" <> "\n"
    where
    nameValue = split (Pattern ":") x
  styleAttrs = intercalate ", " (toAttr <$> styles)
