{-# LANGUAGE ForeignFunctionInterface #-}

#include <bzlib.h>
#include <bindings.dsl.h>

module Data.Conduit.BZlib.Internal where

import Foreign.Storable (Storable (..))
import Foreign.Ptr (Ptr, FunPtr, plusPtr)
import Foreign.C.Types (CUInt (..), CChar (..), CInt (..))

-- #num BZ_RUN
c'BZ_RUN :: CInt
c'BZ_RUN = 0

-- #num BZ_FLUSH
c'BZ_FLUSH :: CInt
c'BZ_FLUSH = 1

-- #num BZ_FINISH
c'BZ_FINISH :: CInt
c'BZ_FINISH = 2

-- #num BZ_OK
c'BZ_OK :: CInt
c'BZ_OK = 0

-- #num BZ_RUN_OK
c'BZ_RUN_OK :: CInt
c'BZ_RUN_OK = 1

-- #num BZ_FLUSH_OK
c'BZ_FLUSH_OK :: CInt
c'BZ_FLUSH_OK = 2

-- #num BZ_FINISH_OK
c'BZ_FINISH_OK :: CInt
c'BZ_FINISH_OK = 3

-- #num BZ_STREAM_END
c'BZ_STREAM_END :: CInt
c'BZ_STREAM_END = 4

-- #num BZ_SEQUENCE_ERROR
c'BZ_SEQUENCE_ERROR :: CInt
c'BZ_SEQUENCE_ERROR = (-1)

-- #num BZ_PARAM_ERROR
c'BZ_PARAM_ERROR :: CInt
c'BZ_PARAM_ERROR = (-2)

-- #num BZ_MEM_ERROR
c'BZ_MEM_ERROR :: CInt
c'BZ_MEM_ERROR = (-3)

-- #num BZ_DATA_ERROR
c'BZ_DATA_ERROR :: CInt
c'BZ_DATA_ERROR = (-4)

-- #num BZ_DATA_ERROR_MAGIC
c'BZ_DATA_ERROR_MAGIC :: CInt
c'BZ_DATA_ERROR_MAGIC = (-5)

-- #num BZ_IO_ERROR
c'BZ_IO_ERROR :: CInt
c'BZ_IO_ERROR = (-6)

-- #num BZ_UNEXPECTED_EOF
c'BZ_UNEXPECTED_EOF :: CInt
c'BZ_UNEXPECTED_EOF = (-7)

-- #num BZ_OUTBUFF_FULL
c'BZ_OUTBUFF_FULL :: CInt
c'BZ_OUTBUFF_FULL = (-8)

-- #num BZ_CONFIG_ERROR
c'BZ_CONFIG_ERROR :: CInt
c'BZ_CONFIG_ERROR = (-9)


-- #starttype bz_stream
-- #field next_in,        Ptr CChar
-- #field avail_in,       CUInt
-- #field total_in_lo32,  CUInt
-- #field total_in_hi32,  CUInt
-- #field next_out,       Ptr CChar
-- #field avail_out,      CUInt
-- #field total_out_lo32, CUInt
-- #field total_out_hi32, CUInt
-- #field state,          Ptr ()
-- #field bzalloc,        Ptr ()
-- #field bzfree,         Ptr ()
-- #field opaque,         Ptr ()
-- #stoptype

data C'bz_stream = C'bz_stream {
  c'bz_stream'next_in :: Ptr CChar,
  c'bz_stream'avail_in :: CUInt,
  c'bz_stream'total_in_lo32 :: CUInt,
  c'bz_stream'total_in_hi32 :: CUInt,
  c'bz_stream'next_out :: Ptr CChar,
  c'bz_stream'avail_out :: CUInt,
  c'bz_stream'total_out_lo32 :: CUInt,
  c'bz_stream'total_out_hi32 :: CUInt,
  c'bz_stream'state :: Ptr (),
  c'bz_stream'bzalloc :: Ptr (),
  c'bz_stream'bzfree :: Ptr (),
  c'bz_stream'opaque :: Ptr ()
} deriving (Eq, Show)

instance Storable C'bz_stream where
    sizeOf    _ = (#size bz_stream)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        next_in <- (#peek bz_stream, next_in) ptr
        avail_in <- (#peek bz_stream, avail_in) ptr
        total_in_lo32 <- (#peek bz_stream, total_in_lo32) ptr
        total_in_hi32 <- (#peek bz_stream, total_in_hi32) ptr
        next_out <- (#peek bz_stream, next_out) ptr
        avail_out <- (#peek bz_stream, avail_out) ptr
        total_out_lo32 <- (#peek bz_stream, total_out_lo32) ptr
        total_out_hi32 <- (#peek bz_stream, total_out_hi32) ptr
        state <- (#peek bz_stream, state) ptr
        bzalloc <- (#peek bz_stream, bzalloc) ptr
        bzfree <- (#peek bz_stream, bzfree) ptr
        opaque <- (#peek bz_stream, opaque) ptr
        return  (C'bz_stream {
          c'bz_stream'next_in = next_in,
          c'bz_stream'avail_in = avail_in,
          c'bz_stream'total_in_lo32 = total_in_lo32,
          c'bz_stream'total_in_hi32 = total_in_hi32,
          c'bz_stream'next_out = next_out,
          c'bz_stream'avail_out = avail_out,
          c'bz_stream'total_out_lo32 = total_out_lo32,
          c'bz_stream'total_out_hi32 = total_out_hi32,
          c'bz_stream'state = state,
          c'bz_stream'bzalloc = bzalloc,
          c'bz_stream'bzfree = bzfree,
          c'bz_stream'opaque = opaque
        })
    poke ptr (C'bz_stream {
      c'bz_stream'next_in = next_in,
      c'bz_stream'avail_in = avail_in,
      c'bz_stream'total_in_lo32 = total_in_lo32,
      c'bz_stream'total_in_hi32 = total_in_hi32,
      c'bz_stream'next_out = next_out,
      c'bz_stream'avail_out = avail_out,
      c'bz_stream'total_out_lo32 = total_out_lo32,
      c'bz_stream'total_out_hi32 = total_out_hi32,
      c'bz_stream'state = state,
      c'bz_stream'bzalloc = bzalloc,
      c'bz_stream'bzfree = bzfree,
      c'bz_stream'opaque = opaque
    }) = do
        (#poke bz_stream, next_in) ptr next_in
        (#poke bz_stream, avail_in) ptr avail_in
        (#poke bz_stream, total_in_lo32) ptr total_in_lo32
        (#poke bz_stream, total_in_hi32) ptr total_in_hi32
        (#poke bz_stream, next_out) ptr next_out
        (#poke bz_stream, avail_out) ptr avail_out
        (#poke bz_stream, total_out_lo32) ptr total_out_lo32
        (#poke bz_stream, total_out_hi32) ptr total_out_hi32
        (#poke bz_stream, state) ptr state
        (#poke bz_stream, bzalloc) ptr bzalloc
        (#poke bz_stream, bzfree) ptr bzfree
        (#poke bz_stream, opaque) ptr opaque

p'bz_stream'next_in :: Ptr C'bz_stream -> Ptr (Ptr CChar)
p'bz_stream'next_in p = plusPtr p (#offset bz_stream, next_in)

p'bz_stream'avail_in :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'avail_in p = plusPtr p (#offset bz_stream, avail_in)

p'bz_stream'total_in_lo32 :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'total_in_lo32 p = plusPtr p (#offset bz_stream, total_in_lo32)

p'bz_stream'total_in_hi32 :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'total_in_hi32 p = plusPtr p (#offset bz_stream, total_in_hi32)

p'bz_stream'next_out :: Ptr C'bz_stream -> Ptr (Ptr CChar)
p'bz_stream'next_out p = plusPtr p (#offset bz_stream, next_out)

p'bz_stream'avail_out :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'avail_out p = plusPtr p (#offset bz_stream, avail_out)

p'bz_stream'total_out_lo32 :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'total_out_lo32 p = plusPtr p (#offset bz_stream, total_out_lo32)

p'bz_stream'total_out_hi32 :: Ptr C'bz_stream -> Ptr CUInt
p'bz_stream'total_out_hi32 p = plusPtr p (#offset bz_stream, total_out_hi32)

p'bz_stream'state :: Ptr C'bz_stream -> Ptr (Ptr ())
p'bz_stream'state p = plusPtr p (#offset bz_stream, state)

p'bz_stream'bzalloc :: Ptr C'bz_stream -> Ptr (Ptr ())
p'bz_stream'bzalloc p = plusPtr p (#offset bz_stream, bzalloc)

p'bz_stream'bzfree :: Ptr C'bz_stream -> Ptr (Ptr ())
p'bz_stream'bzfree p = plusPtr p (#offset bz_stream, bzfree)

p'bz_stream'opaque :: Ptr C'bz_stream -> Ptr (Ptr ())
p'bz_stream'opaque p = plusPtr p (#offset bz_stream, opaque)

#ccall BZ2_bzCompressInit, Ptr <bz_stream> -> CInt -> CInt -> CInt -> IO CInt
#ccall BZ2_bzCompress, Ptr <bz_stream> -> CInt -> IO CInt
#ccall BZ2_bzCompressEnd, Ptr <bz_stream> -> IO CInt

#ccall BZ2_bzDecompressInit, Ptr <bz_stream> -> CInt -> CInt -> IO CInt
#ccall BZ2_bzDecompress, Ptr <bz_stream> -> IO CInt
#ccall BZ2_bzDecompressEnd, Ptr <bz_stream> -> IO CInt
