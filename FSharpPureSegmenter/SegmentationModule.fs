module SegmentationModule

open SegmentModule
open TiffModule

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Map<Segment, Segment>

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    match segmentation.TryFind(segment) with
    | Some(parentSegment: Segment) -> findRoot segmentation parentSegment
    | None -> segment

let rec getTree (segmentation: Segmentation) segment : Segment list =
    seq {
        match segmentation.TryFind(segment) with
        | Some(parentSegment: Segment) -> yield! getTree segmentation parentSegment
        | None -> yield segment
    } |> Seq.toList

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =
    let createPixel (coordinate: Coordinate) : Segment =
        Pixel(coordinate, getColourBands image coordinate)
    createPixel

// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    let neighboursFunctionOuter (segmentation:Segmentation): Segment -> Set<Segment> =
        let neighboursFunction (segment:Segment) : Set<Segment> =
            let segments = [
                for x = 0 to N do
                    for y = 0 to N do
                        yield pixelMap(x, y)
            ]
            let boxedRoot x = findRoot segmentation x
            let pixels = segments
                        |> List.map boxedRoot
                        |> List.filter(fun x -> x <> segment)
                        |> Set.ofList
            pixels
            
        neighboursFunction
    neighboursFunctionOuter

// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    let bestNeighbourFunctionOuter (segmentation:Segmentation) : Segment -> Set<Segment> =
        let neighboursFunction = neighbours segmentation
        let bestNeighbourFunction (segment:Segment) : Set<Segment> =
            let neighboursSet = neighboursFunction segment
            let getCost x = mergeCost x segment
            let validCost x = getCost x < threshold
            let validNeighboursFunction = Seq.filter validCost
            let validNeighbours = validNeighboursFunction neighboursSet
            if Seq.isEmpty validNeighbours then
                Set.ofList []
            else
                let costList = List.map getCost
                let minimumCost = List.min (costList (List.ofSeq validNeighbours))
                let bestNeighbours = Seq.filter(fun x -> (getCost x = minimumCost))
                Set.ofSeq (bestNeighbours validNeighbours)
        bestNeighbourFunction
    bestNeighbourFunctionOuter

// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =
    let tryGrowOneSegmentFunctionOuter (segmentation:Segmentation) : Coordinate -> Segmentation =
        let neighboursFunction = bestNeighbours segmentation
        let rec tryGrowOneSegmentFunction (coordinate: Coordinate) : Segmentation =
            let pixel = pixelMap coordinate
            let rootSegment = findRoot segmentation pixel
            let neighbours = neighboursFunction rootSegment
            if Set.isEmpty neighbours then
                segmentation
            else
                let isMutualBestNeighbour x =
                    let bestNeighbourNeighbours = neighboursFunction x
                    Set.contains rootSegment bestNeighbourNeighbours
                let mutualBestNeighbour =  neighbours |> Set.filter isMutualBestNeighbour
                if Set.isEmpty mutualBestNeighbour then
                    let getCoordinate x =
                        let coordinates = SegmentModule.getCoordinates (Seq.head x)
                        Seq.head coordinates
                    tryGrowOneSegmentFunction (getCoordinate neighbours)
                else
                    let chosenMutualNeighbour = Seq.head mutualBestNeighbour
                    let updatedSegmentation = segmentation.Add(rootSegment, Parent(rootSegment, chosenMutualNeighbour))
                                                        .Add(chosenMutualNeighbour, Parent(rootSegment, chosenMutualNeighbour))
                    updatedSegmentation
        tryGrowOneSegmentFunction
    tryGrowOneSegmentFunctionOuter


// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let tryGrowAllCoordinates (segmentation: Segmentation) : Segmentation =
        let growFunction = tryGrowPixel segmentation
        let coordinates = DitherModule.coordinates N
        let coordinateRetrieval x = Seq.item x coordinates
        let rec tryGrowNextCoordinate (segmentation: Segmentation) (i:int) : Segmentation =
            if i >= Seq.length coordinates then
                segmentation
            else
                tryGrowNextCoordinate (growFunction (coordinateRetrieval i)) (i + 1)
        tryGrowNextCoordinate segmentation 0
    tryGrowAllCoordinates

// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange (segmentation:Segmentation) : Segmentation =
        let changedSegmentation = tryGrowAllCoordinates segmentation
        if changedSegmentation = segmentation then
            changedSegmentation
        else
            growUntilNoChange changedSegmentation
    growUntilNoChange


// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    raise (System.NotImplementedException())
    // Fixme: use the functions above to help implement this function