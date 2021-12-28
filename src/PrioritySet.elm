module PrioritySet exposing (..)

{-| My custom PrioritySet, wrapping PriorityQueue.
It ensures that no value can be inserted twice.
-}

import PriorityQueue exposing (PriorityQueue)
import Set exposing (Set)


type PrioritySet a b
    = PrioritySet { seen : Set a, queue : PriorityQueue ( a, b ) }


empty : PrioritySet comparable Int
empty =
    PrioritySet { seen = Set.empty, queue = PriorityQueue.empty Tuple.second }


{-| Insert a value with a priority into the PrioritySet. Skips already seen values.
A more correct implementation of this would be not to skip already seen values if they
have lower/better priorities. In other words, perhaps a PriorityDict would make more sense.
-}
insert : comparable -> Int -> PrioritySet comparable Int -> PrioritySet comparable Int
insert value priority ((PrioritySet { seen, queue }) as prioritySet) =
    if Set.member value seen then
        prioritySet

    else
        PrioritySet
            { seen = Set.insert value seen
            , queue = PriorityQueue.insert ( value, priority ) queue
            }


head : PrioritySet comparable Int -> Maybe ( comparable, Int )
head (PrioritySet { queue }) =
    PriorityQueue.head queue


tail : PrioritySet comparable Int -> PrioritySet comparable Int
tail (PrioritySet { seen, queue }) =
    PrioritySet
        { seen = seen
        , queue = PriorityQueue.tail queue
        }
