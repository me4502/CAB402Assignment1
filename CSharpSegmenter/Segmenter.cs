using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSharpSegmenter
{
    public class Segmenter
    {
        private Dictionary<Segment, Segment> segmentation = new Dictionary<Segment, Segment>();

        private TiffImage Image;
        private int N;
        private float Threshold;

        public Segmenter(TiffImage image, int N, float threshold)
        {
            this.Image = image;
            this.N = N;
            this.Threshold = threshold;
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
            var segmentCoordinates = segment.GetContainedCoordinates().ToHashSet();

            return segmentCoordinates
                .SelectMany(x => x.GetNeighbouringCoordinates())
                .Where(coord => coord.X < Math.Pow(2, N) && coord.Y < Math.Pow(2, N) && coord.X >= 0 && coord.Y >= 0)
                .Where(coord => !segmentCoordinates.Contains(coord))
                .Select(GetPixelAt)
                .Select(FindRoot)
                .Where(x => !x.Equals(segment))
                .Distinct()
                .ToList();
        }

        public HashSet<Segment> GetBestNeighbours(Segment segment)
        {
            var neighbours = GetNeighbouringSegments(segment)
                .Where(x => segment.MergeCost(x) <= Threshold)
                .ToList();
            if (neighbours.Count > 0)
            {
                var minCost = neighbours.Min(x => x.MergeCost(segment));
                return neighbours.Where(x => x.MergeCost(segment) == minCost).ToHashSet();
            }
            else
            {
                return new HashSet<Segment>();
            }
        }

        public bool TryGrowOneSegment(Coordinate coord)
        {
            var queue = new Queue<Coordinate>();
            var changed = false;
            queue.Enqueue(coord);
            while (queue.Count > 0 && !changed)
            {
                var coordinate = queue.Dequeue();
                var rootSegment = FindRoot(GetPixelAt(coordinate));
                var neighbours = GetBestNeighbours(rootSegment);
                if (neighbours.Count > 0)
                {
                    var mutualNeighbours = neighbours.Where(x => GetBestNeighbours(x).Contains(rootSegment)).ToList();
                    if (mutualNeighbours.Count > 0)
                    {
                        var chosenMutualNeighbour = mutualNeighbours.First();
                        var mutualParent = new Parent(rootSegment, chosenMutualNeighbour);
                        segmentation.Add(rootSegment, mutualParent);
                        segmentation.Add(chosenMutualNeighbour, mutualParent);
                        changed = true;
                        break;
                    }
                    else
                    {
                        queue.Enqueue(neighbours.First().GetContainedCoordinates().First());
                    }
                }
            }

            return changed;
        }

        public bool TryGrowAllCoordinates()
        {
            var coordinates = Dither.coordinates(N);
            var changed = false;
            foreach (Coordinate coord in coordinates)
            {
                if (TryGrowOneSegment(coord))
                {
                    changed = true;
                }
            }

            return changed;
        }

        public void GrowUntilNoChange()
        {
            while (TryGrowAllCoordinates()) ;
        }

        public Segment Segment(Coordinate coordinate)
        {
            GrowUntilNoChange();
            return FindRoot(GetPixelAt(coordinate));
        }
    }
}
