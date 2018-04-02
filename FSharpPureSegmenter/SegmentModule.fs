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

let rec getPixels (segment: Segment) : float list list =
    seq {
        match segment with
        | Parent(parent1, parent2) -> 
            yield! getPixels parent1
            yield! getPixels parent2
        | Pixel(coordinate, colour) -> yield (colour |> List.map float)
    } |> Seq.toList    

// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    let pixels = getPixels segment
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

    let segment1StdDev = stddev segment1 |> List.sum
    let segment2StdDev = stddev segment2 |> List.sum
    let segment3StdDev = stddev segment3 |> List.sum

    let segment1Size = float(getPixels(segment1).Length)
    let segment2Size = float(getPixels(segment2).Length)
    let segment3Size = float(getPixels(segment3).Length)

    let weightedStdDev1 = segment1StdDev * segment1Size
    let weightedStdDev2 = segment2StdDev * segment2Size

    let combinedWeighted = weightedStdDev1 + weightedStdDev2
    let combinedStdDev = (segment3StdDev * segment3Size) - combinedWeighted

    combinedStdDev