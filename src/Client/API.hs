{-# LANGUAGE NoImplicitPrelude #-}

module Client.API where

import SharedTypes

import Language.Fay.FFI
import Language.Fay.Prelude

-- | Call a command.
call :: (Record a,Foreign a) => (Returns a -> Command) -> (a -> Fay ()) -> Fay ()
call f g = ajaxCommand (f Returns) g

-- | Run the AJAX command.
ajaxCommand :: (Record a,Foreign a) => Command -> (a -> Fay ()) -> Fay ()
ajaxCommand = ffi "jQuery['ajax']({\
                  \ \"url\": '/json', \
                  \ \"type\": 'POST', \
                  \ \"data\": { \"json\": JSON.stringify(%1) }, \
                  \ \"dataType\": 'json', \
                  \ \"success\" : %2 \
                  \})"
