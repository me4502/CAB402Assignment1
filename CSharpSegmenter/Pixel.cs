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

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            return Coords.Equals((obj as Pixel).Coords);
        }

        public override int GetHashCode()
        {
            return Coords.GetHashCode();
        }
    }
}
