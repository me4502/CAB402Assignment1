module Program

[<EntryPoint>]
let main argv =
    // load a Tiff image
    let image = TiffModule.loadImage "C:\workspaces\CAB402Assignment1\TestImages\L15-3709E-1833N-Q4.tif"

    // testing using sub-image of size 32x32 pixels
    let N = 3

    // increasing this threshold will result in more segment merging and therefore fewer final segments
    let threshold = 800.0

    // determine the segmentation for the (top left corner of the) image (2^N x 2^N) pixels
    let segmentation = SegmentationModule.segment image N threshold

    // draw the (top left corner of the) original image but with the segment boundaries overlayed in blue
    TiffModule.overlaySegmentation image "segmented.tif" N segmentation

    0 // return an integer exit code