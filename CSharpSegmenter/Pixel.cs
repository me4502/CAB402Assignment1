using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    public class Pixel : Segment
    {
        public Coordinate Coords { get; }
        public byte[] Colour { get; }

        public Pixel(Coordinate coords, byte[] colour)
        {
            this.Coords = coords;
            this.Colour = colour;
        }
    }
}
