module Abacus.UI.History
    ( History
    , singleton
    , present
    , back
    , forward
    , append
    , amend
    )
where

-- Holds a non-empty sequence of events partitioned into past, present, and
-- future.
data History a = History
    { past :: [a]
    , present :: a
    , future :: [a]
    }

-- Starts a new history.
singleton :: a -> History a
singleton x = History { past = [], present = x, future = [] }

-- Moves back one event, or does nothing if the history is already at the
-- beginning.
back :: History a -> History a
back h@History { past = [] } = h
back History { past = p : ps, present = x, future = fs } = History
    { past = ps
    , present = p
    , future = x : fs
    }

-- Moves forward one event, or does nothing if the history is already at the
-- end.
forward :: History a -> History a
forward h@History { future = [] } = h
forward History { past = ps, present = x, future = f : fs } = History
    { past = x : ps
    , present = f
    , future = fs
    }

-- Appends the event to the end of the history and moves time forward to the
-- end.
append :: a -> History a -> History a
append x h = h
    { past = future h ++ present h : past h
    , present = x
    , future = []
    }

-- Amends the present event.
amend :: a -> History a -> History a
amend x h = h { present = x }
