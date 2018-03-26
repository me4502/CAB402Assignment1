module TiffModule

open BitMiracle.LibTiff.Classic

type Image = { width: int; height: int; raster: int array }


let createImage w h r = 
    { width = w; height = h; raster = r}


// return 32 bit representation of colour of pixel (x,y) in ABGR byte order
let getColour image (x,y) = 
    image.raster.[y*image.width+x]


// return list of colour components for pixel (x,y), with one entry for each colour band: red, green and blue
let getColourBands image (x,y) =
    let abgr = getColour image (x,y)
    [(byte (Tiff.GetR abgr)); (byte (Tiff.GetG abgr)); (byte (Tiff.GetB abgr))]


 // create a new image by loading an existing tiff file using BitMiracle Tiff library for .NET
let loadImage filename =
    let image = Tiff.Open(filename, "r")
    let w = (image.GetField TiffTag.IMAGEWIDTH).[0].ToInt()
    let h = (image.GetField TiffTag.IMAGELENGTH).[0].ToInt()
    let r = Array.zeroCreate (w * h)
    image.ReadRGBAImage(w, h, r) |> ignore
    createImage w h r


// write current image to file using BitMiracle Tiff library for .NET
let saveImage image filename =
    let file = Tiff.Open(filename, "w")
    
    // set image properties first ...
    file.SetField(TiffTag.IMAGEWIDTH,image.width) |> ignore
    file.SetField(TiffTag.IMAGELENGTH,image.height) |> ignore
    file.SetField(TiffTag.SAMPLESPERPIXEL,4) |> ignore
    file.SetField(TiffTag.COMPRESSION,Compression.LZW) |> ignore
    file.SetField(TiffTag.BITSPERSAMPLE,8) |> ignore
    file.SetField(TiffTag.ROWSPERSTRIP,1) |> ignore
    file.SetField(TiffTag.ORIENTATION,Orientation.BOTLEFT) |> ignore
    file.SetField(TiffTag.PLANARCONFIG,PlanarConfig.CONTIG) |> ignore
    file.SetField(TiffTag.PHOTOMETRIC, Photometric.RGB) |> ignore
    
    let getPixelByte y i =
        let x = i/4
        let band = i%4
        let bands = getColourBands image (x,y)
        let alpha = 0xFFuy
        match band with // rgba order
        | 3 -> alpha
        | _ -> bands.[band]

    // create an array of bytes encoding of the given row
    let createByteArray row = 
        Array.init (image.width*4) (getPixelByte row)

    // write each of the rows to the file 
    [0 .. (image.height-1)] 
    |> List.map (fun row -> file.WriteScanline(createByteArray row, row)) 
    |> List.reduce (&&)
    |> ignore

    // ensure all writes are flushed to the file
    file.Close()


let BLUE = 0xFFFF0000 // ABGR

// draw the (top left corner of the) original image but with the segment boundaries overlayed in blue
let overlaySegmentation image filename N segmentation =
    let width = 1 <<< N
    let height = 1 <<< N

    // draw blue if it is on the boundary of a segment
    let getOverlayColour i =
        let y = i / width;
        let x = i % width;
        if x = width-1  || x = 0 || segmentation (x,y) <> segmentation (x-1,y) || 
           y = height-1 || y = 0 || segmentation (x,y) <> segmentation (x,y-1) then
            BLUE
        else
            getColour image (x,y)
    let newArray = Array.init (width * height) getOverlayColour    
    let newImage = createImage width  height  newArray  
    saveImage newImage filename