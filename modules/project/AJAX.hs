-- | An example of how AJAX can be done in Fay. This module actually
--   imports the real SharedTypes and Client.API modules used to run
--   this IDE. So any requests you make are actual real requests to
--   the server. You can inspect their source in the “Internal
--   modules” section on this page.

module AJAX where

import SharedTypes
import Client.API

import Language.Fay.JQuery
import Language.Fay.Prelude

main =
  ready $ do
    body <- select "body"
    ul <- select "<ul></ul>" >>= appendTo body
    let tell name = select "<li></li>" >>= appendTo ul >>= setText name
    call ProjectModules $ \(ModuleList modules) -> forM_ modules tell
    call LibraryModules $ \(ModuleList modules) -> forM_ modules tell
