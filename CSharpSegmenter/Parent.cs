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

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
            {
                return false;
            }

            Parent parent = obj as Parent;

            return (parent.Left == Left && parent.Right == Right) || (parent.Right == Left && parent.Left == Right);
        }

        public override int GetHashCode()
        {
            return new Tuple<Segment, Segment>(Left, Right).GetHashCode();
        }
    }
}
