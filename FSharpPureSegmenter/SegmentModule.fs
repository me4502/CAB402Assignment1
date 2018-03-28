module SegmentModule

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment 

let averageLists (lists: float list * float list) : float list =
    raise (System.NotImplementedException())

let rec stddev_rec (segment: Segment) : float list = 
    match segment with
        Parent(parent1, parent2) -> averageLists(stddev_rec(parent1), stddev_rec(parent2))

    // let square x = x * x
    // let stddevPixel seg = 
    //    let mean = seg |> List.average
    //    let variance = seg |> List.averageBy (fun x -> square(x - mean))
    //    sqrt(variance)

    // let averageTogether seg = 
    //    List.average |> seg



// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    stddev_rec(segment)


// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    raise (System.NotImplementedException())
    // Fixme: add implementation here