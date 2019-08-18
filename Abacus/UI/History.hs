module Abacus.UI.History
    ( History
    , singleton
    , present
    , back
    , forward
    , end
    , insert
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

-- Moves to the end of the history.
end :: History a -> History a
end h@History { future = [] } = h
end h = History
    { past = reverse (init $ future h) ++ present h : past h
    , present = last $ future h
    , future = []
    }

-- Inserts an event after the present event, and moves history forward to the
-- new event.
insert :: a -> History a -> History a
insert x h = History
    { past = present h : past h
    , present = x
    , future = future h
    }

-- Amends the present event.
amend :: a -> History a -> History a
amend x h = h { present = x }
