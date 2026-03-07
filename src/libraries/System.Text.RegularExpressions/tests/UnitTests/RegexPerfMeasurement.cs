// Perf measurement: time regex parse/optimize across all real-world patterns
// Temporary - not to be committed to the PR

using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text.Json;
using System.Text.RegularExpressions;
using Xunit;
using Xunit.Abstractions;

namespace System.Text.RegularExpressions.Tests
{
    public class RegexPerfMeasurement
    {
        private readonly ITestOutputHelper _output;

        public RegexPerfMeasurement(ITestOutputHelper output) => _output = output;

        private string[] LoadPatterns()
        {
            string jsonPath = Path.Combine(
                Path.GetDirectoryName(typeof(RegexPerfMeasurement).Assembly.Location)!,
                "Regex_RealWorldPatterns.json");

            if (!File.Exists(jsonPath))
            {
                // Fallback: look in the source tree
                jsonPath = @"C:\git\runtime\src\libraries\System.Text.RegularExpressions\tests\UnitTests\Regex_RealWorldPatterns.json";
            }

            string text = File.ReadAllText(jsonPath);
            int arrayStart = text.IndexOf('[');
            string json = text.Substring(arrayStart);

            using var doc = JsonDocument.Parse(json);
            var set = new HashSet<string>();
            foreach (var elem in doc.RootElement.EnumerateArray())
            {
                set.Add(elem.GetProperty("Pattern").GetString()!);
            }

            string[] patterns = new string[set.Count];
            set.CopyTo(patterns);
            return patterns;
        }

        [Fact]
        public void MeasureParseTime()
        {
            string[] patterns = LoadPatterns();
            _output.WriteLine($"Loaded {patterns.Length} unique patterns");

            // Warmup (5 rounds)
            for (int w = 0; w < 5; w++)
            {
                foreach (string p in patterns)
                {
                    try { RegexParser.Parse(p, RegexOptions.None, CultureInfo.InvariantCulture); } catch { }
                }
            }

            var times = new List<long>();
            for (int iter = 0; iter < 7; iter++)
            {
                var sw = Stopwatch.StartNew();
                foreach (string p in patterns)
                {
                    try { RegexParser.Parse(p, RegexOptions.None, CultureInfo.InvariantCulture); } catch { }
                }
                sw.Stop();
                times.Add(sw.ElapsedMilliseconds);
                _output.WriteLine($"Iteration {iter + 1}: {sw.ElapsedMilliseconds} ms ({(double)sw.ElapsedMilliseconds / patterns.Length:F4} ms/pattern)");
            }

            times.Sort();
            long median = times[times.Count / 2];
            _output.WriteLine($"\nMedian: {median} ms ({(double)median / patterns.Length:F4} ms/pattern) over {patterns.Length} patterns");
        }

        [Fact]
        public void ProfilePhases()
        {
            string[] patterns = LoadPatterns();
            _output.WriteLine($"Loaded {patterns.Length} unique patterns");

            // Warmup
            for (int w = 0; w < 5; w++)
            {
                foreach (string p in patterns)
                {
                    try { RegexParser.Parse(p, RegexOptions.None, CultureInfo.InvariantCulture); } catch { }
                }
            }

            // Reset counters
            RegexNode.s_findLoopsAtomicTicks = 0;
            RegexNode.s_eliminateEndingTicks = 0;
            RegexNode.s_finalReduceTicks = 0;
            RegexNode.s_preReduceTicks = 0;

            var sw = Stopwatch.StartNew();
            foreach (string p in patterns)
            {
                try { RegexParser.Parse(p, RegexOptions.None, CultureInfo.InvariantCulture); } catch { }
            }
            sw.Stop();

            double freq = Stopwatch.Frequency;
            double totalMs = sw.ElapsedTicks / freq * 1000;
            double findLoopsMs = RegexNode.s_findLoopsAtomicTicks / freq * 1000;
            double elimEndMs = RegexNode.s_eliminateEndingTicks / freq * 1000;
            double finalReduceMs = RegexNode.s_finalReduceTicks / freq * 1000;
            double preReduceMs = RegexNode.s_preReduceTicks / freq * 1000;
            double otherMs = totalMs - findLoopsMs - elimEndMs - finalReduceMs - preReduceMs;

            _output.WriteLine($"Total parse time: {totalMs:F1} ms");
            _output.WriteLine($"  PreOptimize FinalReduce: {preReduceMs:F1} ms ({preReduceMs / totalMs * 100:F1}%)");
            _output.WriteLine($"  FindAndMakeLoopsAtomic: {findLoopsMs:F1} ms ({findLoopsMs / totalMs * 100:F1}%)");
            _output.WriteLine($"  EliminateEndingBacktracking: {elimEndMs:F1} ms ({elimEndMs / totalMs * 100:F1}%)");
            _output.WriteLine($"  PostOptimize FinalReduce: {finalReduceMs:F1} ms ({finalReduceMs / totalMs * 100:F1}%)");
            _output.WriteLine($"  Other (parse + ReduceMinimal): {otherMs:F1} ms ({otherMs / totalMs * 100:F1}%)");
        }
    }
}
