module PrioritySet exposing (..)

{-| My custom PrioritySet, wrapping PriorityQueue.
It ensures that no value can be inserted twice.
-}

import PriorityQueue exposing (Priority, PriorityQueue)
import Set exposing (Set)


type PrioritySet a
    = PrioritySet { seen : Set a, queue : PriorityQueue a }


empty : Priority comparable -> PrioritySet comparable
empty priority =
    PrioritySet { seen = Set.empty, queue = PriorityQueue.empty priority }


insert : comparable -> PrioritySet comparable -> PrioritySet comparable
insert value ((PrioritySet { seen, queue }) as prioritySet) =
    if Set.member value seen then
        prioritySet

    else
        PrioritySet
            { seen = Set.insert value seen
            , queue = PriorityQueue.insert value queue
            }


head : PrioritySet comparable -> Maybe comparable
head (PrioritySet { queue }) =
    PriorityQueue.head queue


tail : PrioritySet comparable -> PrioritySet comparable
tail (PrioritySet { seen, queue }) =
    PrioritySet
        { seen = seen
        , queue = PriorityQueue.tail queue
        }
