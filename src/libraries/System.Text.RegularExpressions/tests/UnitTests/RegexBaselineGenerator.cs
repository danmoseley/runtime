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

        /// <summary>
        /// EXPERIMENT 2: FinalOptimize pass ordering sensitivity.
        /// The two main FinalOptimize passes are:
        ///   A = FindAndMakeLoopsAtomic
        ///   B = EliminateEndingBacktracking
        /// Current order is A→B. This experiment tests B→A to see if any patterns
        /// produce a different (better or worse) tree.
        ///
        /// We parse each pattern twice (since FinalOptimize mutates in place), once with
        /// A→B and once with B→A, then compare the resulting trees.
        /// </summary>
        [Fact]
        public void FinalOptimizeOrderingSensitivityExperiment()
        {
            // Define the two orderings: current (A→B) and reversed (B→A)
            int[][] orderings = new[]
            {
                new[] { 0, 1 }, // A→B (current)
                new[] { 1, 0 }, // B→A (reversed)
            };
            string[] orderingNames = new[] { "A→B (current)", "B→A (reversed)" };

            var findings = new List<string>();
            int totalPatterns = 0, totalDifferent = 0, totalParseErrors = 0;

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                totalPatterns++;

                try
                {
                    string[] trees = new string[orderings.Length];
                    for (int i = 0; i < orderings.Length; i++)
                    {
                        // Parse fresh each time (FinalOptimize mutates the tree)
                        // We need to parse WITHOUT running FinalOptimize, then run it manually.
                        // Unfortunately, RegexParser.Parse always calls FinalOptimize.
                        // So instead, we parse normally (gets A→B tree), then for the B→A case,
                        // we parse, re-reduce, and run B→A.
                        //
                        // Actually, we can't easily prevent FinalOptimize from running.
                        // Instead: parse (runs Reduce + FinalOptimize with A→B), capture tree.
                        // For alternate ordering: parse, then re-reduce + run B→A as extra passes.
                        // This isn't a clean test of pure ordering, but it does test whether
                        // running passes in a different order after the initial parse changes anything.
                        //
                        // Better approach: Parse once (gets current A→B tree). Then from that tree,
                        // re-reduce + run passes again in different orders. Compare all results.
                        RegexTree tree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                        RegexNode root = tree.Root;

                        if (i > 0)
                        {
                            // For non-default orderings: re-reduce the tree, then apply the alternate order
                            root.ReReduceTree();
                            root.RunFinalOptimizePassesInOrder(orderings[i]);
                        }

                        trees[i] = FlattenTree(root.ToString());
                    }

                    // Compare: does the tree differ when we add B→A on top of the standard pipeline?
                    if (trees[0] != trees[1])
                    {
                        totalDifferent++;
                        findings.Add($"ORDERING_DIFF|{Escape(pattern)}|{options}|{orderingNames[0]}: {trees[0]}|{orderingNames[1]}: {trees[1]}");
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
            string outputPath = Path.Combine(Path.GetTempPath(), "regex_ordering_results.txt");
            var resultLines = new List<string>
            {
                $"=== FINALOPTIMIZE PASS ORDERING SENSITIVITY EXPERIMENT ===",
                $"Total patterns: {totalPatterns}",
                $"Parse errors (skipped): {totalParseErrors}",
                $"Patterns where ordering matters: {totalDifferent}",
                $"",
                $"Orderings tested:",
                $"  Default: {orderingNames[0]} (FindAndMakeLoopsAtomic → EliminateEndingBacktracking)",
                $"  Alternate: {orderingNames[1]} (EliminateEndingBacktracking → FindAndMakeLoopsAtomic)",
                $"  Alternate is run AFTER the default pipeline (parse → reduce → FinalOptimize),",
                $"  then re-reduce + B→A. This tests whether running passes in a different order",
                $"  on top of the standard pipeline finds additional optimizations.",
                $"",
                $"=== FINDINGS ===",
                $""
            };
            resultLines.AddRange(findings);
            File.WriteAllLines(outputPath, resultLines);

            _output.WriteLine($"Total patterns: {totalPatterns}");
            _output.WriteLine($"Parse errors: {totalParseErrors}");
            _output.WriteLine($"Patterns where ordering matters: {totalDifferent}");
            _output.WriteLine($"Results: {outputPath}");

            if (findings.Count > 0)
            {
                _output.WriteLine("");
                _output.WriteLine("=== FINDINGS ===");
                foreach (string f in findings.Take(30))
                {
                    _output.WriteLine(f);
                }
                if (findings.Count > 30)
                {
                    _output.WriteLine($"... and {findings.Count - 30} more");
                }
            }
        }

        /// <summary>
        /// EXPERIMENT 2b: Clean pass ordering comparison.
        /// Parses each pattern into a Reduce-only tree (no FinalOptimize), then
        /// applies FinalOptimize passes in each possible ordering and compares results.
        ///
        /// Since we can't prevent FinalOptimize from running during Parse, we instead
        /// test whether running the 2 passes in B→A order vs A→B order from the
        /// SAME starting point (post-parse/reduce, pre-FinalOptimize) would differ.
        ///
        /// Approach: We can't easily skip FinalOptimize, so we test both orderings
        /// as additional passes on the already-optimized tree AND test whether
        /// running JUST B (without A first) then A gives different results than A then B.
        /// </summary>
        [Fact]
        public void FinalOptimizeOrderingCleanExperiment()
        {
            // For each pattern, we parse twice (each parse runs the standard A→B pipeline).
            // Then:
            //   Tree1: the standard tree (A→B result)
            //   Tree2: parse again, but after standard pipeline, re-reduce and run B→A
            //   Tree3: parse again, after standard pipeline, re-reduce and run A→B again
            //   Tree4: parse again, after standard pipeline, re-reduce and run B only, then re-reduce and run A only
            //
            // Compare Tree2 vs Tree3: does the re-run ordering matter?
            // Compare Tree1 vs Tree3: does re-running A→B change anything? (same as Experiment 1)

            var findings = new List<string>();
            int totalPatterns = 0, totalOrderingMatters = 0, totalParseErrors = 0;

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                totalPatterns++;

                try
                {
                    // Tree with standard pipeline (A→B)
                    RegexTree tree1Parse = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    string tree1 = FlattenTree(tree1Parse.Root.ToString());

                    // Tree with re-reduce → B→A
                    RegexTree tree2Parse = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    tree2Parse.Root.ReReduceTree();
                    tree2Parse.Root.RunFinalOptimizePassesInOrder(new[] { 1, 0 }); // B→A
                    string tree2 = FlattenTree(tree2Parse.Root.ToString());

                    // Tree with re-reduce → A→B (same order as default, but an extra round)
                    RegexTree tree3Parse = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    tree3Parse.Root.ReReduceTree();
                    tree3Parse.Root.RunFinalOptimizePassesInOrder(new[] { 0, 1 }); // A→B
                    string tree3 = FlattenTree(tree3Parse.Root.ToString());

                    // Does the ordering of the SECOND round matter?
                    if (tree2 != tree3)
                    {
                        totalOrderingMatters++;
                        findings.Add($"ORDER_MATTERS|{Escape(pattern)}|{options}|ReReduce+AB: {tree3}|ReReduce+BA: {tree2}");
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

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_ordering_clean_results.txt");
            var resultLines = new List<string>
            {
                $"=== CLEAN PASS ORDERING EXPERIMENT ===",
                $"Total patterns: {totalPatterns}",
                $"Parse errors (skipped): {totalParseErrors}",
                $"Patterns where re-run ordering matters (ReReduce+AB ≠ ReReduce+BA): {totalOrderingMatters}",
                $"",
                $"This tests whether the ORDER of the second-round FinalOptimize passes matters.",
                $"All patterns start from the same state (standard parse + reduce + FinalOptimize).",
                $"Then we re-reduce and run the two FinalOptimize passes in each order.",
                $"",
                $"=== FINDINGS ===",
                $""
            };
            resultLines.AddRange(findings);
            File.WriteAllLines(outputPath, resultLines);

            _output.WriteLine($"Total patterns: {totalPatterns}");
            _output.WriteLine($"Parse errors: {totalParseErrors}");
            _output.WriteLine($"Ordering matters: {totalOrderingMatters}");
            _output.WriteLine($"Results: {outputPath}");

            if (findings.Count > 0)
            {
                _output.WriteLine("");
                foreach (string f in findings.Take(20))
                {
                    _output.WriteLine(f);
                }
                if (findings.Count > 20)
                {
                    _output.WriteLine($"... and {findings.Count - 20} more");
                }
            }
        }
    }
}