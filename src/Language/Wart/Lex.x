{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-imports
    -fno-warn-unused-matches #-}
module Language.Wart.Lex (lex) where

import Control.Category
import Control.Lens
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Internal (w2c)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word
import Language.Wart.Loc
import Language.Wart.Parser
import Language.Wart.Pos
import Language.Wart.Token
import Prelude (Either (..), Integral (..), Maybe (..), Num (..), Ord (..),
                ($), (&&), (||),
                fromIntegral, undefined)
}

$alpha = [a-zA-Z_]
$numeric = [0-9]

@name = $alpha [$alpha $numeric]*

:-

$white+ ;
"var" { lexVar }
"fn" { lexFn }
"for" { lexFor }
"while" { lexWhile }
"{" { lexLeftBrace }
"}" { lexRightBrace }
"(" { lexLeftParen }
")" { lexRightParen }
";" { lexSemicolon }
@name { lexName }

{
lex :: MonadParser m => Parser m (Located Token)
lex = do
  s@(ParserState pos xs) <- get
  case alexScan s 0 of
    AlexToken s'@(ParserState pos' _) n m -> put s' >> m (Loc pos pos') xs n
    AlexError (ParserState pos' _) -> throwLexError $ Loc pos pos'
    AlexEOF -> return $ Located (Loc pos pos) EOF
    AlexSkip s' _ -> put s' >> lex

lexVar :: AlexAction
lexVar l _ _ = return $ Located l Var

lexFn :: AlexAction
lexFn l _ _ = return $ Located l Fn

lexFor :: AlexAction
lexFor l _ _ = return $ Located l For

lexWhile :: AlexAction
lexWhile l _ _ = return $ Located l While

lexLeftBrace :: AlexAction
lexLeftBrace l _ _ = return $ Located l LeftBrace

lexRightBrace :: AlexAction
lexRightBrace l _ _ = return $ Located l RightBrace

lexLeftParen :: AlexAction
lexLeftParen l _ _ = return $ Located l LeftParen

lexRightParen :: AlexAction
lexRightParen l _ _ = return $ Located l RightParen

lexSemicolon :: AlexAction
lexSemicolon l _ _ = return $ Located l Semicolon

lexName :: AlexAction
lexName l xs n = Located l . Name <$> getName (ByteString.take n xs)

type AlexAction =
  forall m . MonadParser m =>
  Loc -> ByteString -> Int -> Parser m (Located Token)

type AlexInput = ParserState

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = \ case
  ParserState pos xs -> case ByteString.uncons xs of
    Nothing -> Nothing
    Just (x, ys) -> Just (x, ParserState (plusPos (w2c x) pos) ys)

plusPos :: Char -> Pos -> Pos
plusPos = \ case
  '\t' -> _2 %~ ((+ 1) . (* 8) . (`div` 8) . (+ 7))
  '\n' -> _1 +~ 1
  _ -> _2 +~ 1

getName :: MonadParser m => ByteString -> Parser m Text
getName = Text.decodeUtf8' >>> \ case
  Right a -> return a
  Left e -> do
    ParserState pos _ <- get
    throwUnicodeException (Loc pos pos) e
}
