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

import ICFPC2020.AST
  
import Debug.Trace

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

parseFunction :: Parser Value 
parseFunction = do
  _ <- parseAp
  funName <- parseIdentifier
  arg <- parseValue
  let funRepresent = [arg]
  let funApply = \x -> Nothing
  return $ VFunction Function {..}

parseVariable :: Parser Value
parseVariable = do
  name <- parseIdentifier
  return $ VVariable name

parseValue :: Parser Value
parseValue = parseNumber <|> parseFunction <|> parseVariable

parseStmt :: Parser Declaration
parseStmt = do
  lhs <- parseIdentifier
  rhs <- parseValue
  return (lhs, rhs)

parseProgram :: Parser [Declaration]
parseProgram = do
  ret <- many parseStmt
  return ret

--parseFunction :: ParserFunction
--parseFunction = parseFunctionName <$> [
--  "inc", "dec", "add", "mul",
--  "div", "eq", "lt", "mod",
--  "dem", "send", "neg", "s",
--  "c", "b", "t", "f",
--  "pwr2", "i", "cons", "car", "cdr",
--  "nil", "isnil", "vec", "draw", "checkerboard",
--  "multipledraw", "send", "if0", "interact"]
