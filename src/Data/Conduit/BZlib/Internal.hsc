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

#starttype bz_stream
#field next_in,        Ptr CChar
#field avail_in,       CUInt
#field total_in_lo32,  CUInt
#field total_in_hi32,  CUInt
#field next_out,       Ptr CChar
#field avail_out,      CUInt
#field total_out_lo32, CUInt
#field total_out_hi32, CUInt
#field state,          Ptr ()
#field bzalloc,        Ptr ()
#field bzfree,         Ptr ()
#field opaque,         Ptr ()
#stoptype

#ccall BZ2_bzCompressInit, Ptr <bz_stream> -> CInt -> CInt -> CInt -> IO CInt
#ccall BZ2_bzCompress, Ptr <bz_stream> -> CInt -> IO CInt
#ccall BZ2_bzCompressEnd, Ptr <bz_stream> -> IO CInt

#ccall BZ2_bzDecompressInit, Ptr <bz_stream> -> CInt -> CInt -> IO CInt
#ccall BZ2_bzDecompress, Ptr <bz_stream> -> IO CInt
#ccall BZ2_bzDecompressEnd, Ptr <bz_stream> -> IO CInt
