module CmdsParser (parse) where

import           Cmds
import           Control.Arrow
import qualified Text.Parsec        as P
import           Text.Parsec.Char
import           Text.Parsec.String
import           Types

parse :: Input -> Either Error Cmd
parse input =
    left show res
    where res = P.parse cmdsParser "" input

-- | Commands parser
cmdsParser :: Parser Cmd
cmdsParser = loadParser

-- TODO: Tokenize the parser!
loadParser :: Parser Cmd
loadParser = spaces *> (string "load" >> return (Load []))
