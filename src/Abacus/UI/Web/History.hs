module Abacus.UI.Web.History
    ( History
    , singleton
    , now
    , back
    , forward
    , append
    , amend
    )
where

-- Holds a non-empty sequence of events partitioned into past, present, and
-- future.
data History a = History [a] a [a]

-- Starts a new history.
singleton :: a -> History a
singleton x = History [] x []

-- Returns the present event.
now :: History a -> a
now (History _ x _) = x

-- Moves back one event, or does nothing if the history is already at the
-- beginning.
back :: History a -> History a
back (History [] x future) = History [] x future
back (History (x : past) y future) = History past x (y : future)

-- Moves forward one event, or does nothing if the history is already at the
-- end.
forward :: History a -> History a
forward (History past x []) = History past x []
forward (History past x (y : future)) = History (x : past) y future

-- Appends the event to the end of the history and moves time forward to the
-- end.
append :: a -> History a -> History a
append x (History past y future) = History (future ++ y : past) x []

-- Amends the present event.
amend :: a -> History a -> History a
amend x (History past _ future) = History past x future
