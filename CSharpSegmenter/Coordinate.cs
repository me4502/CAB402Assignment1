using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    public class Coordinate
    {
        public int X { get; }
        public int Y { get; }

        public Coordinate(int x, int y)
        {
            this.X = x;
            this.Y = y;
        }

        public List<Coordinate> GetNeighbouringCoordinates()
        {
            return new List<Coordinate>
            {
                new Coordinate(X - 1, Y),
                new Coordinate(X + 1, Y),
                new Coordinate(X, Y - 1),
                new Coordinate(X, Y + 1)
            };
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            Coordinate other = obj as Coordinate;

            return other.X == X && other.Y == Y;
        }

        public override int GetHashCode()
        {
            return new Tuple<int, int>(X, Y).GetHashCode();
        }
    }
}
