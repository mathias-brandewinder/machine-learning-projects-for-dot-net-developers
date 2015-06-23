using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    public class FunctionalExample
    {
        private IEnumerable<Observation> data;

        private readonly Func<int[], int[], int> distance;

        public FunctionalExample(Func<int[], int[], int> distance)
        {
            this.distance = distance;
        }

        public Func<int[], int[], int> Distance
        {
            get { return this.distance; }
        }

        public void Train(IEnumerable<Observation> trainingSet)
        {
            this.data = trainingSet;
        }

        public string Predict(int[] pixels)
        {
            Observation currentBest = null;
            var shortest = Double.MaxValue;

            foreach (Observation obs in this.data)
            {
                var dist = this.Distance(obs.Pixels, pixels);
                if (dist < shortest)
                {
                    shortest = dist;
                    currentBest = obs;
                }
            }

            return currentBest.Label;
        }
    }
}
