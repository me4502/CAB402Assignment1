using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSharpSegmenter
{
    class Segmenter
    {
        private Dictionary<Segment, Segment> segmentation = new Dictionary<Segment, Segment>();

        private TiffImage Image;
        private int N;

        public Segmenter(TiffImage image, int N)
        {
            this.Image = image;
            this.N = N;
        }

        public Segment FindRoot(Segment segment)
        {
            while (segmentation.ContainsKey(segment))
            {
                segment = segmentation.GetValueOrDefault(segment);
            }

            return segment;
        }

        public Pixel GetPixelAt(Coordinate coordinate)
        {
            return new Pixel(coordinate, Image.getColourBands(coordinate.X, coordinate.Y));
        }

        public List<Segment> GetNeighbouringSegments(Segment segment)
        {
            return segment.GetContainedCoordinates()
                .SelectMany(x => x.GetNeighbouringCoordinates())
                .Where(x => true)
                .ToList();
        }
    }
}
