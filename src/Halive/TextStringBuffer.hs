module Halive.TextStringBuffer where

import StringBuffer
import Foreign.Marshal.Utils

textToStringBuffer :: Text -> StringBuffer
textToStringBuffer str =

    unsafePerformIO $ do
        bytes <- Text.encodeUtf8 str
        let size = BS.length bytes

        buf <- mallocForeignPtrArray (size+3)
        withForeignPtr buf $ \ptr -> do
            -- utf8EncodeString ptr str
            unsafeUseAsCString bytes $ \(charPtr, _) ->
                copyBytes ptr charPtr size
            pokeArray (ptr `plusPtr` size :: Ptr Word8) [0,0,0]
            -- sentinels for UTF-8 decoding
        return (StringBuffer buf size 0)
