module ICFPC2020.IO
  ( parseProgram
  ) where

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM

import ICFPC2020.AST
import ICFPC2020.Operations

parseIdentifier :: Parser BS.ByteString
parseIdentifier = do
  idt <- APC.takeWhile $ inClass ":a-zA-Z0-9_"
  return $ idt

parseNumber :: Parser Value
parseNumber = do
  num <- signed decimal
  return $ VNumber num

parseAp :: Parser Value
parseAp = string "ap" $> VAp

parseNil :: Parser Value
parseNil = string "nil" $> VNil

parseReference :: Parser Value
parseReference = do
  name <- parseIdentifier
  return $ case HM.lookup name builtins of
    Just f -> VFunction f
    Nothing -> VMacro name

parseValue :: Parser Value
parseValue = parseNumber <|> parseAp <|> parseNil <|> parseReference

parseExpression :: Parser [Value]
parseExpression = parseValue `sepBy1` takeWhile1 (== ' ')

parseMacro :: Parser Macro
parseMacro = do
  lhs <- parseIdentifier
  skipWhile (== ' ')
  _ <- char '='
  skipWhile (== ' ')
  rhs <- parseExpression
  return $ (lhs, rhs)

parseProgram :: Parser Program
parseProgram = do
  skipSpace
  ms <- parseMacro `sepBy1` endOfLine
  skipSpace
  _ <- endOfInput
  return $ Program { macros = HM.fromList ms }
