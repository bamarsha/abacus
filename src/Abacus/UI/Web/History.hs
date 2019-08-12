module Abacus.UI.Web.History
    ( History
    , empty
    , now
    , back
    , forward
    , record
    )
where

-- Holds a sequence of events and the current point in time (which partitions
-- the events into before and after).
data History a = History [a] [a]

-- The empty history.
empty :: History a
empty = History [] []

-- Returns Just the event immediately following the current point in time, or
-- Nothing if there are no events after.
now :: History a -> Maybe a
now (History _ []) = Nothing
now (History _ (x : _)) = Just x

-- Moves the current point in time back one event.
back :: History a -> History a
back (History [] after) = History [] after
back (History (x : before) after) = History before (x : after)

-- Moves the current point in time forward one event.
forward :: History a -> History a
forward (History before (x : after)) = History (x : before) after
forward (History before []) = History before []

-- Records a new event after the most recent in the history and moves the
-- current point in time to just after the new event.
record :: a -> History a -> History a
record x (History before after) = History (x : after ++ before) []
