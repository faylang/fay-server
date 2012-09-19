{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Dates.

module Language.Fay.Date where

import Language.Fay.FFI
import Language.Fay.Prelude

data Date
instance Foreign Date

-- | Get the current date.
getCurrentTime :: Fay Date
getCurrentTime = ffi "new Date()"

-- | Format a date to an ISO string.
toISOString :: Date -> String
toISOString = ffi "%1['toISOString']()"

-- | Format a date with the given format string.
formatDate :: String -> Date -> String
formatDate = ffi "%2['toString'](%1)"
