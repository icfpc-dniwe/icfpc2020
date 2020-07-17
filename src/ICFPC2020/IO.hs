module ICFPC2020.IO
  ( parseProgram
  ) where

import Control.Monad
import Data.Functor
import Data.List
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict as HM

import ICFPC2020.AST
  
import Debug.Trace

parseBuiltinFunctionIdentifier :: Parser String
parseBuiltinFunctionIdentifier = string <$> [
    "inc", "dec", "add", "mul",
    "div", "eq", "lt", "mod",
    "dem", "send", "neg", "s",
    "c", "b", "t", "f",
    "pwr2", "i", "cons", "car", "cdr",
    "nil", "isnil", "vec", "draw", "checkerboard",
    "multipledraw", "send", "if0", "interact"
  ]

parseIdentifier :: Parser String
parseIdentifier = do
  idt <- APC.takeWhile $ inClass ":a-zA-Z0-9_"
  return $ BS.unpack idt

parseNumber :: Parser Value
parseNumber = do
  num <- signed decimal
  return $ VNumber num

parseAp :: Parser Value
parseAp = string "ap" $> VAp

parseBuiltinFunction :: Parser Value 
parseBuiltinFunction = do
  funName <- parseBuiltinFunctionIdentifier
  let funRepresent = []
  let funApply = \x -> Nothing
  return $ VFunction Function {..}

parseVariable :: Parser Value
parseVariable = do
  name <- parseIdentifier
  return $ VVariable name

parseValue :: Parser Value
parseValue = parseNumber <|> parseBuiltinFunction <|> parseVariable

parseMacro :: Parser Macro
parseMacro = do
  lhs <- parseIdentifier
  rhs <- many $ parseValue `sepBy1` char ' '
  return (lhs, rhs)

parseProgram :: Parser Program
parseProgram = do
  ms <- many $ parseMacro `sepBy1` endOfLine
  return $ HM.fromList ms
