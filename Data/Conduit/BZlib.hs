module Data.Conduit.BZlib (
  compress,
  decompress,
  
  bzip2,
  bunzip2,
  
  CompressParams(..),
  DecompressParams(..),
  def,
  ) where

import Control.Applicative
import Control.Monad as CM
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import Data.Conduit
import Data.Default
import Data.Maybe
import Data.IORef
import Foreign
import Foreign.C

import Data.Conduit.BZlib.Internal

-- | Compression parameters
data CompressParams
  = CompressParams
    { cpBlockSize  :: Int -- ^ Compress level [1..9]. default is 9.
    , cpVerbosity  :: Int -- ^ Verbosity mode [0..4]. default is 0.
    , cpWorkFactor :: Int -- ^ Work factor [0..250]. default is 30.
    }

instance Default CompressParams where
  def = CompressParam 9 0 30

-- | Decompression parameters
data DecompressParams
  = DecompressParams
    { dpVerbosity :: Int -- ^ Verbosity mode [0..4]. default is 0
    , dpSmall     :: Int -- ^ If this is nonzero, use an algorithm uses less memory but slow. default is 0
    }

instance Default DecompressParams where
  def = CompressParam 0 0

bufSize :: Int
bufSize = 4096

getAvailOut :: Ptr C'bz_stream -> IO (Maybe S.ByteString)
getAvailOut ptr = do
  availOut <- liftIO $ fromIntegral <$> (peek $ p'bz_stream'avail_out ptr)
  if availOut < bufSize
    then do
      let len = bufSize - availOut
      p <- (`plusPtr` (-len)) <$> (peek $ p'bz_stream'next_out ptr)
      out <- S.packCStringLen (p, fromIntegral len)
      poke (p'bz_stream'next_out ptr) p
      poke (p'bz_stream'avail_out ptr) (fromIntegral bufSize)
      return $ Just out
    else do
    return Nothing

fillInput :: Ptr C'bz_stream -> IORef (Ptr CChar, Int) -> S.ByteString -> IO ()
fillInput ptr mv bs = S.unsafeUseAsCStringLen bs $ \(p, len) -> do
  (buf, bsize) <- readIORef mv
  let nsize = head [ s | x <- [0..], let s = bsize * 2 ^ x, s >= len ]
  nbuf <- if nsize >= bsize then reallocBytes buf nsize else return buf
  copyBytes nbuf p len
  poke (p'bz_stream'avail_in ptr) $ fromIntegral len
  poke (p'bz_stream'next_in ptr) nbuf
  writeIORef mv (nbuf, nsize)

throwIfMinus :: String -> IO CInt -> IO CInt
throwIfMinus s m = do
  r <- m
  when (r < 0) $ monadThrow $ userError $ s ++ ": " ++ show r
  return r

throwIfMinus_ :: String -> IO CInt -> IO ()
throwIfMinus_ s m = CM.void $ throwIfMinus s m

-- | Compress a stream of ByteStrings.
compress
  :: MonadResource m
     => CompressParams -- ^ Compress parameter
     -> Conduit S.ByteString m S.ByteString
compress CompressParams {..} = do
  (_, ptr)    <- lift $ allocate malloc free
  (_, inbuf)  <- lift $ allocate (mallocBytes bufSize >>= \p -> newIORef (p, bufSize))
                                 (\mv -> readIORef mv >>= \(p, _) -> free p)
  (_, outbuf) <- lift $ allocate (mallocBytes bufSize) free
  liftIO $ poke ptr $ C'bz_stream
    { c'bz_stream'next_in        = nullPtr
    , c'bz_stream'avail_in       = 0
    , c'bz_stream'total_in_lo32  = 0
    , c'bz_stream'total_in_hi32  = 0
    , c'bz_stream'next_out       = outbuf
    , c'bz_stream'avail_out      = fromIntegral bufSize
    , c'bz_stream'total_out_lo32 = 0
    , c'bz_stream'total_out_hi32 = 0
    , c'bz_stream'state          = nullPtr
    , c'bz_stream'bzalloc        = nullPtr
    , c'bz_stream'bzfree         = nullPtr
    , c'bz_stream'opaque         = nullPtr
    }
  _ <- lift $ allocate
    (throwIfMinus_ "bzCompressInit" $ c'BZ2_bzCompressInit ptr cpBlockSize cpVerbosityLevel cpWorkFactor)
    (\_ -> throwIfMinus_ "bzCompressEnd" $ c'BZ2_bzCompressEnd ptr)
  go ptr inbuf
  where
    go ptr inbuf = do
      mbinp <- await
      case mbinp of
        Just inp | not (S.null inp) -> do
          inSize' <- liftIO $ fillInput ptr inbuf inp
          yields ptr c'BZ_RUN
          go ptr inbuf
        Just _ -> do
          go ptr inbuf
        Nothing -> do
          yields ptr c'BZ_FINISH

    yields ptr action = do
      cont <- liftIO $ throwIfMinus "bzCompress" $ c'BZ2_bzCompress ptr action
      mbout <- liftIO $ getAvailOut ptr
      when (isJust mbout) $
        yield $ fromJust mbout
      availIn <- liftIO $ fromIntegral <$> (peek $ p'bz_stream'avail_in ptr)
      when (availIn > 0 || action == c'BZ_FINISH && cont /= c'BZ_STREAM_END) $
        yields ptr action

-- | Decompress a stream of ByteStrings.
decompress
  :: MonadResource m
     => DecompressParams -- ^ Decompress parameter
     -> Conduit S.ByteString m S.ByteString
decompress DecompressParams {..} = do
  (_, ptr)    <- lift $ allocate malloc free
  (_, inbuf)  <- lift $ allocate (mallocBytes bufSize >>= \p -> newIORef (p, bufSize))
                                 (\mv -> readIORef mv >>= \(p, _) -> free p)
  (_, outbuf) <- lift $ allocate (mallocBytes bufSize) free
  liftIO $ poke ptr $ C'bz_stream
    { c'bz_stream'next_in        = nullPtr
    , c'bz_stream'avail_in       = 0
    , c'bz_stream'total_in_lo32  = 0
    , c'bz_stream'total_in_hi32  = 0
    , c'bz_stream'next_out       = outbuf
    , c'bz_stream'avail_out      = fromIntegral bufSize
    , c'bz_stream'total_out_lo32 = 0
    , c'bz_stream'total_out_hi32 = 0
    , c'bz_stream'state          = nullPtr
    , c'bz_stream'bzalloc        = nullPtr
    , c'bz_stream'bzfree         = nullPtr
    , c'bz_stream'opaque         = nullPtr
    }
  _ <- lift $ allocate
    (throwIfMinus_ "bzDecompressInit" $ c'BZ2_bzDecompressInit ptr dpVerbosityLevel dpSmall)
    (\_ -> throwIfMinus_ "bzDecompressEnd" $ c'BZ2_bzDecompressEnd ptr)
  go ptr inbuf
  where
    go ptr inbuf = do
      mbinp <- await
      case mbinp of
        Just inp | not (S.null inp) -> do
          liftIO $ fillInput ptr inbuf inp
          cont <- yields ptr
          when cont $ go ptr inbuf
        Just _ -> do
          go ptr inbuf
        Nothing -> do
          lift $ monadThrow $ userError "unexpected EOF on decompress"
    
    decomp ptr = liftIO $ do
      ret <- throwIfMinus "BZ2_bzDecompress" $ c'BZ2_bzDecompress ptr
      return $ ret == c'BZ_OK

    yields ptr = do
      cont <- decomp ptr
      mbout <- liftIO $ getAvailOut ptr
      when (isJust mbout) $
        yield $ fromJust mbout
      availIn <- liftIO $ fromIntegral <$> (peek $ p'bz_stream'avail_in ptr)
      if availIn > 0
        then yields ptr
        else return cont

-- | bzip2 compression with default parameters.
bzip2 :: MonadResource m => Conduit S.ByteString m S.ByteString
bzip2 = compress def

-- | bzip2 decompression with default parameters.
bunzip2 :: MonadResource m => Conduit S.ByteString m S.ByteString
bunzip2 = decompress def
