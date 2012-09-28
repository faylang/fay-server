{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-type-defaults #-}

-- | The client.

module Language.Fay.ReactiveMvc where

import Language.Fay.FFI
import Language.Fay.Guid
import Language.Fay.Prelude
import Language.Fay.Ref

--------------------------------------------------------------------------------
-- Reactive API

-- Model

-- | A model.
data Model a = Model
  { modelGuid      :: Guid
  , modelValue     :: Ref a
  , modelCallbacks :: Ref [Callback a]
  }

-- | A callback on a model's events.
data Callback a = Callback Guid EventType (a -> Fay ())
instance Foreign a => Foreign (Callback a)

-- | An event type a callback can be called for.
data EventType = Change | Delete
  deriving (Eq)

-- | Make a new model containing the given value.
newModel :: Foreign a => a -> Fay (Model a)
newModel value = do
  guid <- newGuid
  value_ref <- newRef value
  callbacks_ref <- newRef []
  return (Model guid value_ref callbacks_ref)

-- | Delete the given model. This signals delete to all the subscribed
-- views.
deleteModel :: Foreign a => Model a -> Fay ()
deleteModel (Model _ value_ref callbacks_ref) = do
  value <- readRef value_ref
  callbacks <- readRef callbacks_ref
  forM_ callbacks $ \(Callback _ typ action) ->
    when (typ == Delete) $
      action value
  writeRef callbacks_ref []

-- | Put a new value in the model, this signals a change to all the
-- subscribed views.
putModel :: Foreign a => Model a -> a -> Fay ()
putModel (Model _ value_ref callbacks_ref) value = do
  writeRef value_ref value
  callbacks <- readRef callbacks_ref
  forM_ callbacks $ \(Callback _ typ action) ->
    when (typ == Change) $
      action value

-- | Get the value in the model.
getModel :: Foreign a => Model a -> Fay a
getModel (Model _ value_ref _) = do
  readRef value_ref

-- | Modify the given model.
modifyModel :: Foreign a => Model a -> (a -> a) -> Fay ()
modifyModel model f = do
  value <- getModel model
  putModel model (f value)

-- View

-- | A view of a model containing a value.
data View a view = View
  { viewGuid :: Guid
  , viewModel :: Model a
  , viewRender :: a -> Fay view
  }

-- | Make a new view on the given model.
newView :: Model a -> (a -> Fay view) -> Fay (View a view)
newView model render = do
  guid <- newGuid
  return (View guid model render)

-- | Delete the view. This removes all callbacks attached to any
-- models.
deleteView :: Foreign a => View a view -> Fay ()
deleteView (View guid (Model _ _ callbacks_ref) _) = do
  callbacks <- readRef callbacks_ref
  writeRef callbacks_ref
           (filter (\(Callback cguid _ _) -> cguid /= guid)
                   callbacks)

-- | Bind an event handler for the view.
viewReact :: Foreign a => EventType -> View a view -> (a -> Fay ()) -> Fay ()
viewReact typ (View guid (Model _ _ callbacks_ref) _) action = do
  callbacks <- readRef callbacks_ref
  writeRef callbacks_ref (callbacks ++ [Callback guid typ action])

-- | Bind an event handler for the model.
modelReact :: Foreign a => EventType -> Model x -> Model a -> (a -> Fay ()) -> Fay ()
modelReact typ (Model guid _ _) (Model _ _ callbacks_ref) action = do
  callbacks <- readRef callbacks_ref
  writeRef callbacks_ref (callbacks ++ [Callback guid typ action])

-- | Render the view with the model's value.
renderView :: Foreign a => View a view -> Fay view
renderView (View _ (Model _ value_ref _) action) = do
  value <- readRef value_ref
  action value

--------------------------------------------------------------------------------
-- Utilities

(&) :: Fay a -> (a -> Fay b) -> Fay b
x & y = x >>= y
infixl 1 &

instance (Foreign a,Foreign b) => Foreign (a,b)
