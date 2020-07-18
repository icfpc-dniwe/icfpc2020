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

parseBuiltinFunctionIdentifier :: Parser BS.ByteString
parseBuiltinFunctionIdentifier = foldr1 (<|>) $ string <$> sortBy (flip compare) [
    "inc", "dec", "add", "mul",
    "div", "eq", "lt", "mod",
    "dem", "send", "neg", "s",
    "c", "b", "t", "f",
    "pwr2", "i", "cons", "car", "cdr",
    "nil", "isnil", "vec", "draw", "checkerboard",
    "multipledraw", "send", "if0", "interact"
  ]

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

parseBuiltinFunction :: Parser Value 
parseBuiltinFunction = do
  funName' <- parseBuiltinFunctionIdentifier
  let funName = BS.unpack funName'
  let funApply = \x -> []
  return $ VFunction Function {..}

parseMacroExpression :: Parser Value
parseMacroExpression = do
  name <- parseIdentifier
  return $ VMacro name

parseValue :: Parser Value
parseValue = parseNumber <|> parseAp <|> parseBuiltinFunction <|> parseMacroExpression

parseMacro :: Parser Macro
parseMacro = do
  lhs <- parseIdentifier
  _ <- char ' '
  _ <- char '='
  _ <- char ' '
  rhs <- parseValue `sepBy1` char ' '
  return $ (BS.unpack lhs, rhs)

parseProgram :: Parser Program
parseProgram = do
  ms <- parseMacro `sepBy1` endOfLine
  _ <- endOfInput
  return $ HM.fromList ms
