using BitMiracle.LibTiff.Classic;

namespace CSharpSegmenter
{ 
    public class TiffImage
    {
        private int width, height;
        private int[] raster;

        // return 32 bit representation of colour of pixel (x,y) in ABGR byte order
        public int getColour(int x, int y)
        {
            return raster[y * width + x];
        }

        private void setColour(int x, int y, int colour)
        {
            raster[y * width + x] = colour;
        }

        // return list of colour components for pixel (x,y), with one entry for each colour band: red, green and blue
        public byte[] getColourBands(int x, int y)
        {
            var abgr = getColour(x, y);
            return new byte[] { (byte)Tiff.GetR(abgr), (byte)Tiff.GetG(abgr), (byte)Tiff.GetB(abgr) };
        }

        private TiffImage(int width, int height)
        {
            this.width = width;
            this.height = height;
            this.raster = new int[width * height];
        }

        // create a new image by loading an existing tiff file using BitMiracle Tiff library for .NET
        public TiffImage(string filename)
        {
            var file = Tiff.Open(filename, "r");
            width = file.GetField(TiffTag.IMAGEWIDTH)[0].ToInt();
            height = file.GetField(TiffTag.IMAGELENGTH)[0].ToInt();
            raster = new int[width * height];
            file.ReadRGBAImage(width, height, raster);
            file.Close();
        }

        // write current image to file using BitMiracle Tiff library for .NET
        public void saveImage(string filename)
        {
            var file = Tiff.Open(filename, "w");

            // set image properties first ...
            file.SetField(TiffTag.IMAGEWIDTH, width);
            file.SetField(TiffTag.IMAGELENGTH, height);
            file.SetField(TiffTag.SAMPLESPERPIXEL, 4);
            file.SetField(TiffTag.COMPRESSION, Compression.LZW);
            file.SetField(TiffTag.BITSPERSAMPLE, 8);
            file.SetField(TiffTag.ROWSPERSTRIP, 1);
            file.SetField(TiffTag.ORIENTATION, Orientation.BOTLEFT);
            file.SetField(TiffTag.PLANARCONFIG, PlanarConfig.CONTIG);
            file.SetField(TiffTag.PHOTOMETRIC, Photometric.RGB);

            byte ALPHA = 0xFF;

            // write each of the rows to the file 
            for (var y = 0; y < height; y++)
            {
                // create an array of bytes encoding of the given row
                var row = new byte[width * 4];
                for (var x = 0; x < width; x++)
                {
                    var colour = getColourBands(x, y);
                    // rgba order
                    for (var band = 0; band < colour.Length; band++)
                        row[x * 4 + band] = colour[band];
                    row[x * 4 + colour.Length] = ALPHA;
                }
                file.WriteScanline(row, y);
            }

            // ensure all writes are flushed to the file
            file.Close();
        }

        // draw the (top left corner of the) original image but with the segment boundaries overlayed in blue
        public void overlaySegmentation(string filename, int N, Segmenter segmenter)
        {
            var newImage = new TiffImage(1 << N, 1 << N);

            var BLUE = unchecked((int)0xFFFF0000); // ABGR

            for (var y = 0; y < newImage.height; y++)
                for (var x = 0; x < newImage.width; x++)
                {
                    var a = segmenter.Segment(new Coordinate(x, y));
                    var b = segmenter.Segment(new Coordinate(x - 1, y));
                    var c = segmenter.Segment(new Coordinate(x, y - 1));
                    if (x == newImage.width - 1 || x == 0 || !segmenter.Segment(new Coordinate(x, y)).Equals(segmenter.Segment(new Coordinate(x - 1, y))) ||
                        y == newImage.height - 1 || y == 0 || !segmenter.Segment(new Coordinate(x, y)).Equals(segmenter.Segment(new Coordinate(x, y - 1))))
                        newImage.setColour(x, y, BLUE);
                    else
                        newImage.setColour(x, y, getColour(x, y));
                }

            newImage.saveImage(filename);
        }
    }
}
