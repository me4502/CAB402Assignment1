module SegmentationModule

open SegmentModule
open TiffModule
open System.Collections.Generic

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Dictionary<Segment, Segment>

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    if segmentation.ContainsKey(segment) then
        findRoot segmentation (segmentation.GetValueOrDefault segment)
    else
        segment

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =
    let createPixel coordinate = Pixel(coordinate, getColourBands image coordinate)
    createPixel

let neighbourPixels (coordinates:Coordinate list) (N:int) : (Coordinate list) =
    List.map (fun (x,y) -> [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]) coordinates
    |> List.concat 
    |> List.filter (fun (x,y) -> x < (pown 2 N) && y < (pown 2 N) && x >= 0 && y >= 0)
    |> List.filter (fun coord -> not (List.contains coord coordinates))
    |> List.distinct

// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    let neighboursFunction segmentation segment =
        let segments = (neighbourPixels (SegmentModule.getCoordinates segment) N) |> List.map pixelMap
        let boxedRoot x = findRoot segmentation x
        segments
        |> List.map boxedRoot
        |> List.filter(fun x -> x <> segment)
        |> Set.ofList
    neighboursFunction

// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    let bestNeighboursFunction segmentation segment =
        let getCost x = mergeCost x segment
        let validNeighbours = 
            neighbours segmentation segment
            |> Seq.filter (fun x -> getCost x <= threshold)
            |> List.ofSeq
        match validNeighbours with
        | [] -> Set.empty
        | ls -> validNeighbours |> Seq.groupBy getCost |> Seq.minBy fst |> snd |> Set.ofSeq
    bestNeighboursFunction

// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =
    let tryGrowOneSegmentFunctionOuter segmentation =
        let neighboursFunction = bestNeighbours segmentation
        let rec tryGrowOneSegmentFunction coordinate =
            let rootSegment = findRoot segmentation (pixelMap coordinate)
            let neighbours = neighboursFunction rootSegment
            if Set.isEmpty neighbours then
                segmentation
            else
                let isMutualBestNeighbour = neighboursFunction >> Set.contains rootSegment
                let mutualBestNeighbour =  neighbours |> Set.filter isMutualBestNeighbour
                if Set.isEmpty mutualBestNeighbour then
                    let getCoordinate = Seq.head >> SegmentModule.getCoordinates >> Seq.head
                    tryGrowOneSegmentFunction (getCoordinate neighbours)
                else
                    let chosenMutualNeighbour = Seq.head mutualBestNeighbour
                    let mutualParent = Parent(rootSegment, chosenMutualNeighbour)
                    segmentation.Add(rootSegment, mutualParent)
                    segmentation.Add(chosenMutualNeighbour, mutualParent)
                    segmentation
        tryGrowOneSegmentFunction
    tryGrowOneSegmentFunctionOuter


// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let tryGrowAllCoordinates segmentation =
        let coordinates = DitherModule.coordinates N |> List.ofSeq
        List.fold (fun acc coordinate -> tryGrowPixel acc coordinate) segmentation coordinates
    tryGrowAllCoordinates

// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange segmentation =
        let changedSegmentation = tryGrowAllCoordinates segmentation
        if changedSegmentation = segmentation then
            changedSegmentation
        else
            growUntilNoChange changedSegmentation
    growUntilNoChange


// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    let segmentation = new Dictionary<Segment, Segment>();
    let pixelMap = createPixelMap image
    let neighbourFunction = createNeighboursFunction pixelMap N
    let bestNeighboursFunction = createBestNeighbourFunction neighbourFunction threshold
    let tryGrowOneSegmentFunction = createTryGrowOneSegmentFunction bestNeighboursFunction pixelMap
    let tryGrowAllCoordinatesFunction = createTryGrowAllCoordinatesFunction tryGrowOneSegmentFunction N
    let growUntilNoChangeFunction = createGrowUntilNoChangeFunction tryGrowAllCoordinatesFunction
    let segmentFunction coordinate = findRoot (growUntilNoChangeFunction segmentation) (pixelMap coordinate)
    segmentFunction