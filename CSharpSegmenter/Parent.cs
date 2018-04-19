using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter
{
    class Parent : Segment
    {
        private Segment Left { get; }
        private Segment Right { get; }

        public Parent(Segment left, Segment right)
        {
            this.Left = left;
            this.Right = right;
        }

        public List<Segment> GetChildren()
        {
            List<Segment> segments = new List<Segment>();
            if (Left != null)
            {
                segments.Add(Left);
            }
            if (Right != null)
            {
                segments.Add(Right);
            }
            return segments;
        }
    }
}
