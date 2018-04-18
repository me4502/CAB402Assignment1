module SegmentModule

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


let rec getSegments (segment: Segment) : Segment list =
    seq {
        match segment with
        | Parent(parent1, parent2) -> 
            yield! getSegments parent1
            yield! getSegments parent2
        | Pixel(coordinate, colour) -> yield segment
    } |> Seq.toList    


let rec getColours (segment: Segment) : float list list =
    seq {
        match segment with
        | Parent(parent1, parent2) -> 
            yield! getColours parent1
            yield! getColours parent2
        | Pixel(coordinate, colour) -> yield (colour |> List.map float)
    } |> Seq.toList    


let rec getCoordinates (segment: Segment) : Coordinate list =
    seq {
        match segment with
        | Parent(parent1, parent2) -> 
            yield! getCoordinates parent1
            yield! getCoordinates parent2
        | Pixel(coordinate, colour) -> yield coordinate
    } |> Seq.toList    


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
    let segment3 = Parent(segment1, segment2)

    let getSummedStdDev = stddev >> List.sum

    let segment1StdDev = getSummedStdDev segment1
    let segment2StdDev = getSummedStdDev segment2
    let segment3StdDev = getSummedStdDev segment3

    let getSegmentSize = getColours >> List.length >> float

    let segment1Size = getSegmentSize segment1
    let segment2Size = getSegmentSize segment2
    let segment3Size = getSegmentSize segment3

    let weightedStdDev1 = segment1StdDev * segment1Size
    let weightedStdDev2 = segment2StdDev * segment2Size

    let combinedWeighted = weightedStdDev1 + weightedStdDev2
    let combinedStdDev = (segment3StdDev * segment3Size) - combinedWeighted

    combinedStdDev