module SegmentModule

open System.Collections.Generic

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment 

let square (x: float) : float = 
    x * x

// Copied from http://stackoverflow.com/questions/3016139/help-me-to-explain-the-f-matrix-transpose-function
let rec transpose = function
    | (_::_)::_ as M -> 
        List.map List.head M :: transpose (List.map List.tail M)
    | _ -> []


let getSegments (segment: Segment) : (Coordinate * Colour) list =
    let output = new ResizeArray<(Coordinate * Colour)>()
    let searchQueue = new Queue<Segment>()
    searchQueue.Enqueue(segment)
    while searchQueue.Count > 0 do
        let mutable searchSegment = searchQueue.Dequeue();
        match searchSegment with
        | Parent(left, right) -> 
            searchQueue.Enqueue(left)
            searchQueue.Enqueue(right)
        | Pixel(coordinate, colour) -> output.Add ((coordinate, colour))
    output |> Seq.toList


let rec getColours (segment: Segment) : float list list =
    getSegments segment |> List.map snd |> List.map (List.map float)


let rec getCoordinates (segment: Segment) : Coordinate list =
    getSegments segment |> List.map fst  


// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    let pixels = getColours segment
    let transposedColours = transpose pixels

    let calculateStdDev input =
        let mean = input |> List.average
        let variance = input |> List.averageBy (fun x -> square(x - mean))
        sqrt(variance)

    transposedColours |> List.map calculateStdDev

// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    let getSummedStdDev = stddev >> List.sum
    let getSegmentSize = getColours >> List.length >> float
    let getWeightedStdDev segment = (getSummedStdDev segment) * (getSegmentSize segment)
    let getCombinedWeighted seg1 seg2 = getWeightedStdDev seg1 + getWeightedStdDev seg2
    
    let segment3 = Parent(segment1, segment2)
    (getWeightedStdDev segment3) - (getCombinedWeighted segment1 segment2)