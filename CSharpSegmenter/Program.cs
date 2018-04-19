using System;

namespace CSharpSegmenter
{
    class Program
    {
        static void Main(string[] args)
        {
            // load a Tiff image
            var image = new TiffImage(args[0]);

            // testing using sub-image of size 32x32 pixels
            var N = 5;

            // increasing this threshold will result in more segment merging and therefore fewer final segments
            var threshold = 800.0f;

            // determine the segmentation for the (top left corner of the) image (2^N x 2^N) pixels
            var segmentation = new Segmenter(image, N, threshold);

            // draw the (top left corner of the) original image but with the segment boundaries overlayed in blue
            image.overlaySegmentation("segmented.tif", N, segmentation);
        }
    }
}
