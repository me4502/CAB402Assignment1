using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSharpSegmenter
{
    public abstract class Segment
    {
        public List<Pixel> getContainedPixels()
        {
            var output = new List<Pixel>();
            var searchQueue = new Queue<Segment>();
            searchQueue.Enqueue(this);
            while (searchQueue.Count > 0)
            {
                var searchSegment = searchQueue.Dequeue();
                if (searchSegment is Parent)
                {
                    foreach (var child in (searchSegment as Parent).GetChildren())
                    {
                        searchQueue.Enqueue(child);
                    }
                }
                else
                {
                    output.Add(searchSegment as Pixel);
                }
            }
            return output;
        }

        public List<byte[]> GetContainedColours()
        {
            return getContainedPixels().Select(x => x.Colour).ToList();
        }            

        public List<Coordinate> GetContainedCoordinates()
        {
            return getContainedPixels().Select(x => x.Coords).ToList();
        }            

        public List<float> GetStdDev()
        {
            var pixels = GetContainedColours();

            float[] output = new float[] { 0f, 0f, 0f };

            for (int i = 0; i < 3; i++)
            {
                List<float> values = new List<float>();

                foreach (byte[] colourBands in pixels)
                {
                    values.Add(colourBands[i]);
                }

                float mean = values.Average();
                double varianceValue = values.Select(x => Math.Pow(x - mean, 2)).Sum();
                output[i] = (float) Math.Sqrt(varianceValue);
            }

            return output.ToList();
        }

        public float GetWeightedStdDev()
        {
            return GetStdDev().Sum() * getContainedPixels().Count;
        }

        public float MergeCost(Segment otherSegment)
        {
            var parentSegment = new Parent(this, otherSegment);
            return parentSegment.GetWeightedStdDev() - (GetWeightedStdDev() + otherSegment.GetWeightedStdDev());
        }
    }
}
