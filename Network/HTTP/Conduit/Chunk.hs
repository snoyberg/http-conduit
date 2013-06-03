{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Network.HTTP.Conduit.Chunk
    ( chunkedConduit
    , chunkIt
    ) where

import Numeric (showHex)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Blaze.ByteString.Builder.HTTP
import qualified Blaze.ByteString.Builder as Blaze

import Data.Conduit
import qualified Data.Conduit.Binary as CB

import Control.Monad (when, unless)
import Network.HTTP.Conduit.Types (HttpException (InvalidChunkedData))

chunkedConduit :: MonadThrow m
               => Bool -- ^ send the headers as well, necessary for a proxy
               -> Conduit S.ByteString m S.ByteString
chunkedConduit sendHeaders = do
    i <- getLen
    when sendHeaders $ yield $ S8.pack $ showHex i "\r\n"
    CB.isolate i
    dropCRLF
    unless (i == 0) $ chunkedConduit sendHeaders
  where
    getLen = do
        (i, empty) <- start 0 True
        when empty $ monadThrow InvalidChunkedData
        return i
      where
        start i empty = await >>= maybe (return (i, empty)) (go i empty)

        go i empty bs =
            case S.uncons bs of
                Nothing -> start i empty
                Just (13, _) -> do
                    leftover bs
                    dropCRLF
                    return (i, empty)
                Just (59, bs') -> do
                    leftover bs'
                    CB.dropWhile (/= 13)
                    dropCRLF
                    return (i, empty)
                Just (w, bs') ->
                    case toI w of
                        Just i' -> go (i * 16 + i') False bs'
                        Nothing -> monadThrow InvalidChunkedData

        toI w
            | 48 <= w && w <= 57  = Just $ fromIntegral w - 48
            | 65 <= w && w <= 70  = Just $ fromIntegral w - 55
            | 97 <= w && w <= 102 = Just $ fromIntegral w - 87
            | otherwise = Nothing

    dropCRLF = do
        s <- CB.take 2
        when (s /= "\r\n") $ monadThrow InvalidChunkedData

chunkIt :: Monad m => Conduit Blaze.Builder m Blaze.Builder
chunkIt =
    await >>= maybe
        (yield chunkedTransferTerminator)
        (\x -> yield (chunkedTransferEncoding x) >> chunkIt)
