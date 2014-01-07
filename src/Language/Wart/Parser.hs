{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
module Language.Wart.Parser
       ( Parser
       , ParserState (..)
       , runParser
       , MonadParser (..)
       ) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Text.Encoding.Error (UnicodeException)
import Language.Wart.Loc
import Language.Wart.Pos
import Language.Wart.Token
import Prelude (($), (.))

type Parser = StateT ParserState

data ParserState = ParserState {-# UNPACK #-} !Pos {-# UNPACK #-} !ByteString

runParser :: Monad m => Parser m a -> ByteString -> m a
runParser m = evalStateT m . ParserState (Pos 1 1)

class (Applicative m, Monad m) => MonadParser m where
  throwLexError :: Loc -> m a
  throwUnicodeException :: Loc -> UnicodeException -> m a
  throwUnexpectedToken :: Loc -> Token -> m a

#ifndef HLINT
  default throwLexError :: (MonadTrans t, MonadParser m) => Loc -> t m a
  throwLexError = lift . throwLexError
#endif

#ifndef HLINT
  default throwUnicodeException :: (MonadTrans t, MonadParser m)
                                => Loc -> UnicodeException -> t m a
  throwUnicodeException l e = lift $ throwUnicodeException l e
#endif

#ifndef HLINT
  default throwUnexpectedToken :: (MonadTrans t, MonadParser m)
                                => Loc -> Token -> t m a
  throwUnexpectedToken l t = lift $ throwUnexpectedToken l t
#endif

instance MonadParser m => MonadParser (StateT s m)
