﻿using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    class Coordinate
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
            throw new NotImplementedException();
        }
    }
}
