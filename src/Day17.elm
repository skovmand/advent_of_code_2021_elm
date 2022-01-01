module Day17 exposing (parseInput, solvePart1, solvePart2)

{-| Day 17: Trick Shot
<https://adventofcode.com/2021/day/17>

Pretty simple once I got started. In part 2, I tweaked the generated coordinates until I got no more solutions ;-)
Solved in on New Years Eve, after the fireworks stopped.

-}

import SantasList exposing (maybeListOf2)
import Utilities exposing (maybeAll)



---------------
-- PARSE
---------------


type alias TargetArea =
    { xRange : ( Int, Int ), yRange : ( Int, Int ) }


parseInput : String -> Maybe TargetArea
parseInput input =
    input
        |> String.replace "target area: x=" ""
        |> String.replace "y=" ""
        |> String.split ", "
        |> parseXYRanges


parseXYRanges : List String -> Maybe TargetArea
parseXYRanges list =
    case list of
        [ x, y ] ->
            Maybe.map2 TargetArea (parseRange x) (parseRange y)

        _ ->
            Nothing


parseRange : String -> Maybe ( Int, Int )
parseRange range =
    range
        |> String.split ".."
        |> List.map String.toInt
        |> maybeAll
        |> maybeListOf2



-----------------------------
-- PART 1
-----------------------------


type alias Velocity =
    ( Int, Int )


type alias ProbeState =
    { velocity : Velocity, position : ( Int, Int ), maxHeight : Int }


solvePart1 : TargetArea -> Maybe Int
solvePart1 targetArea =
    maximizeInitialYVelocity targetArea


maximizeInitialYVelocity : TargetArea -> Maybe Int
maximizeInitialYVelocity targetArea =
    let
        initialCoords =
            List.range 1 50
                |> List.concatMap
                    (\x -> List.range 1 150 |> List.map (\y -> ( x, y )))
    in
    initialCoords
        |> List.foldl
            (\initialVelocities targetAreaHitAcc ->
                case launchProbe targetArea initialVelocities of
                    HitTarget maxHeight ->
                        maxHeight :: targetAreaHitAcc

                    Overshot ->
                        targetAreaHitAcc
            )
            []
        |> List.maximum



--------------
-- PART 2
--------------


solvePart2 : TargetArea -> Int
solvePart2 targetArea =
    countAllSolutions targetArea


countAllSolutions : TargetArea -> Int
countAllSolutions targetArea =
    let
        initialCoords =
            List.range 0 130
                |> List.concatMap
                    (\x -> List.range -150 150 |> List.map (\y -> ( x, y )))
    in
    initialCoords
        |> List.filter (\coord -> launchProbe targetArea coord /= Overshot)
        |> List.length


type LaunchResult
    = HitTarget Int
    | Overshot


launchProbe : TargetArea -> Velocity -> LaunchResult
launchProbe targetArea initialVelocity =
    { velocity = initialVelocity, position = ( 0, 0 ), maxHeight = 0 }
        |> runLaunchProbeSteps targetArea


runLaunchProbeSteps : TargetArea -> ProbeState -> LaunchResult
runLaunchProbeSteps targetArea probeState =
    let
        nextProbeState =
            step probeState
    in
    if isInTargetArea targetArea nextProbeState then
        HitTarget nextProbeState.maxHeight

    else if isOvershot targetArea nextProbeState then
        Overshot

    else
        runLaunchProbeSteps targetArea nextProbeState


step : ProbeState -> ProbeState
step ({ velocity, position, maxHeight } as probeState) =
    let
        ( velX, velY ) =
            velocity

        ( posX, posY ) =
            position

        newVelX =
            if velX == 0 then
                0

            else if velX < 0 then
                velX + 1

            else
                velX - 1

        newPosY =
            posY + velY

        newMaxHeight =
            if newPosY > maxHeight then
                newPosY

            else
                maxHeight
    in
    { probeState | position = ( posX + velX, newPosY ), velocity = ( newVelX, velY - 1 ), maxHeight = newMaxHeight }


isInTargetArea : TargetArea -> ProbeState -> Bool
isInTargetArea targetArea probeState =
    let
        ( xPos, yPos ) =
            probeState.position

        ( xRangeMin, xRangeMax ) =
            targetArea.xRange

        ( yRangeMin, yRangeMax ) =
            targetArea.yRange
    in
    xPos >= xRangeMin && xPos <= xRangeMax && yPos >= yRangeMin && yPos <= yRangeMax


isOvershot : TargetArea -> ProbeState -> Bool
isOvershot targetArea probeState =
    let
        ( xPos, yPos ) =
            probeState.position

        ( _, xRangeMax ) =
            targetArea.xRange

        ( yRangeMin, _ ) =
            targetArea.yRange
    in
    if xPos > xRangeMax || yPos < yRangeMin then
        True

    else
        False
