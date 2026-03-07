// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// Experimental: captures tree baselines and runs fixed-point convergence experiment.

using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;
using Xunit.Abstractions;

namespace System.Text.RegularExpressions.Tests
{
    public class RegexBaselineGenerator(ITestOutputHelper output)
    {
        private readonly ITestOutputHelper _output = output;

        private static string FlattenTree(string tree)
        {
            var sb = new StringBuilder();
            bool first = true;
            foreach (string rawLine in tree.Split('\n'))
            {
                string line = rawLine.TrimEnd('\r');
                if (!first) sb.Append('|');
                sb.Append(line);
                first = false;
            }
            return sb.ToString();
        }

        /// <summary>
        /// EXPERIMENT 1: Fixed-point convergence test.
        /// For each real-world pattern:
        ///   1. Parse (which runs Reduce on each node during tree construction)
        ///   2. FinalOptimize runs (FindAndMakeLoopsAtomic + EliminateEndingBacktracking + UpdateBumpalong)
        ///   3. Re-run Reduce on every node (ReReduceTree)
        ///   4. Re-run FinalOptimize passes (ReRunFinalOptimizePasses)
        ///   5. Check if tree changed — if so, the current single-pass design missed something
        ///   6. Repeat until stable
        /// </summary>
        [Fact]
        public void FixedPointConvergenceExperiment()
        {
            var findings = new List<string>();
            int totalPatterns = 0, totalChanged = 0, totalParseErrors = 0;
            int maxRoundsNeeded = 0;

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                totalPatterns++;

                try
                {
                    // Standard parse + reduce + FinalOptimize (round 0)
                    RegexTree tree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    RegexNode root = tree.Root;

                    string round0Tree = FlattenTree(root.ToString());

                    // Now attempt additional rounds
                    int round = 0;
                    string previousTree = round0Tree;
                    bool everChanged = false;

                    for (round = 1; round <= 10; round++)
                    {
                        // Re-reduce all nodes
                        bool reduceChanged = root.ReReduceTree();

                        // Re-run FinalOptimize passes
                        bool finalChanged = root.ReRunFinalOptimizePasses();

                        string currentTree = FlattenTree(root.ToString());

                        if (currentTree == previousTree)
                        {
                            // Converged
                            break;
                        }

                        everChanged = true;
                        findings.Add($"ROUND {round}|{Escape(pattern)}|{options}|BEFORE: {previousTree}|AFTER: {currentTree}");
                        previousTree = currentTree;
                    }

                    if (everChanged)
                    {
                        totalChanged++;
                        if (round > maxRoundsNeeded)
                            maxRoundsNeeded = round;
                    }

                    if (round > 10)
                    {
                        findings.Add($"DID NOT CONVERGE|{Escape(pattern)}|{options}|after 10 rounds");
                    }
                }
                catch (RegexParseException)
                {
                    totalParseErrors++;
                }
                catch (Exception ex)
                {
                    findings.Add($"ERROR|{Escape(pattern)}|{options}|{ex.GetType().Name}: {ex.Message}");
                }
            }

            // Write results
            string outputPath = Path.Combine(Path.GetTempPath(), "regex_fixedpoint_results.txt");
            var resultLines = new List<string>
            {
                $"=== FIXED-POINT CONVERGENCE EXPERIMENT ===",
                $"Total patterns: {totalPatterns}",
                $"Parse errors (skipped): {totalParseErrors}",
                $"Patterns that changed with extra rounds: {totalChanged}",
                $"Max rounds needed: {maxRoundsNeeded}",
                $"",
                $"=== FINDINGS (patterns that benefited from additional rounds) ===",
                $""
            };
            resultLines.AddRange(findings);
            File.WriteAllLines(outputPath, resultLines);

            _output.WriteLine($"Total patterns: {totalPatterns}");
            _output.WriteLine($"Parse errors: {totalParseErrors}");
            _output.WriteLine($"Changed with extra rounds: {totalChanged}");
            _output.WriteLine($"Max rounds needed: {maxRoundsNeeded}");
            _output.WriteLine($"Results: {outputPath}");

            // Also write a concise summary of just the changed patterns
            if (findings.Count > 0)
            {
                _output.WriteLine($"");
                _output.WriteLine($"=== FINDINGS ===");
                foreach (string f in findings.Take(50))
                {
                    _output.WriteLine(f);
                }
                if (findings.Count > 50)
                {
                    _output.WriteLine($"... and {findings.Count - 50} more");
                }
            }
        }

        /// <summary>
        /// Generates the flattened tree baseline for all real-world patterns.
        /// </summary>
        [Fact]
        public void GenerateTreeBaseline()
        {
            var results = new List<string>();
            int success = 0, failed = 0;

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                try
                {
                    string tree = FlattenTree(RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture).Root.ToString());
                    results.Add($"{Escape(pattern)}\t{options}\t{tree}");
                    success++;
                }
                catch (Exception ex)
                {
                    results.Add($"{Escape(pattern)}\t{options}\tPARSE_ERROR:{ex.GetType().Name}");
                    failed++;
                }
            }

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_tree_baseline.tsv");
            File.WriteAllLines(outputPath, results);
            _output.WriteLine($"Wrote {success} trees ({failed} errors) to {outputPath}");
        }

        private static string Escape(string s) =>
            s.Replace("\\", "\\\\").Replace("\t", "\\t").Replace("\n", "\\n").Replace("\r", "\\r");
    }
}