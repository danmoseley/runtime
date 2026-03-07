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
                        root.ReReduceTreeForTests();

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
        /// Snapshots the current optimizer output for all patterns to a TSV file.
        /// After making optimizer changes, run VerifyTreeBaseline to see what changed.
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

        /// <summary>
        /// Generates a JSON baseline file mapping each pattern+options to its expected tree.
        /// Output goes to the test directory as real_world_expected_trees.json.
        /// </summary>
        [Fact]
        public void GenerateJsonBaseline()
        {
            var sb = new StringBuilder();
            sb.AppendLine("[");
            int count = 0;

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                string tree;
                try
                {
                    tree = FlattenTree(RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture).Root.ToString());
                }
                catch
                {
                    tree = null;
                }

                if (count > 0) sb.AppendLine(",");
                sb.Append($"  {{\"p\":{JsonEncode(pattern)},\"o\":{options}");
                if (tree != null) sb.Append($",\"t\":{JsonEncode(tree)}");
                sb.Append("}");
                count++;
            }

            sb.AppendLine();
            sb.AppendLine("]");

            string dir = Path.GetDirectoryName(typeof(RegexBaselineGenerator).Assembly.Location);
            // Walk up to find test source directory (where the .csproj is)
            string testDir = dir;
            while (testDir != null && !File.Exists(Path.Combine(testDir, "System.Text.RegularExpressions.Unit.Tests.csproj")))
                testDir = Path.GetDirectoryName(testDir);

            string outputPath = testDir != null
                ? Path.Combine(testDir, "real_world_expected_trees.json")
                : Path.Combine(Path.GetTempPath(), "real_world_expected_trees.json");

            File.WriteAllText(outputPath, sb.ToString(), Encoding.UTF8);
            _output.WriteLine($"Wrote {count} entries to {outputPath}");
        }

        private static string JsonEncode(string s)
        {
            var sb = new StringBuilder("\"");
            foreach (char c in s)
            {
                switch (c)
                {
                    case '"': sb.Append("\\\""); break;
                    case '\\': sb.Append("\\\\"); break;
                    case '\n': sb.Append("\\n"); break;
                    case '\r': sb.Append("\\r"); break;
                    case '\t': sb.Append("\\t"); break;
                    case '\b': sb.Append("\\b"); break;
                    case '\f': sb.Append("\\f"); break;
                    default:
                        if (c < 0x20) sb.Append($"\\u{(int)c:X4}");
                        else sb.Append(c);
                        break;
                }
            }
            sb.Append('"');
            return sb.ToString();
        }

        /// <summary>
        /// Compares current optimizer output against a previously-generated TSV baseline.
        /// Run GenerateTreeBaseline first (before changes), then make your changes, then run this.
        /// Reports: which patterns changed, how many nodes before/after, and the full diff.
        /// </summary>
        [Fact]
        public void VerifyTreeBaseline()
        {
            string baselinePath = Path.Combine(Path.GetTempPath(), "regex_tree_baseline.tsv");
            Assert.True(File.Exists(baselinePath), $"Baseline not found at {baselinePath}. Run GenerateTreeBaseline first.");

            // Load baseline from TSV
            var baseline = new Dictionary<string, string>(); // key: escaped_pattern + \t + options -> tree
            foreach (string line in File.ReadAllLines(baselinePath))
            {
                int lastTab = line.LastIndexOf('\t');
                if (lastTab > 0)
                {
                    string key = line.Substring(0, lastTab);   // escaped_pattern \t options
                    string tree = line.Substring(lastTab + 1);  // flattened tree
                    baseline[key] = tree;
                }
            }

            int total = 0, matched = 0, changed = 0, improved = 0, regressed = 0, noBaseline = 0;
            var changes = new List<string>();

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                total++;

                string key = $"{Escape(pattern)}\t{options}";
                string currentTree;
                try
                {
                    currentTree = FlattenTree(RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture).Root.ToString());
                }
                catch
                {
                    currentTree = "PARSE_ERROR";
                }

                if (!baseline.TryGetValue(key, out string expectedTree))
                {
                    noBaseline++;
                    continue;
                }

                if (currentTree == expectedTree)
                {
                    matched++;
                }
                else
                {
                    changed++;
                    int beforeNodes = expectedTree.Split('|').Length;
                    int afterNodes = currentTree.Split('|').Length;
                    string delta = afterNodes < beforeNodes ? $"FEWER NODES ({beforeNodes}→{afterNodes})" :
                                   afterNodes > beforeNodes ? $"MORE NODES ({beforeNodes}→{afterNodes})" :
                                   $"SAME COUNT ({beforeNodes})";

                    // Simple heuristic: fewer nodes or more atomic = improvement
                    int beforeAtomic = CountSubstring(expectedTree, "loopatomic") + CountSubstring(expectedTree, "Atomic");
                    int afterAtomic = CountSubstring(currentTree, "loopatomic") + CountSubstring(currentTree, "Atomic");
                    bool likelyBetter = afterNodes < beforeNodes || afterAtomic > beforeAtomic;
                    if (likelyBetter) improved++; else regressed++;

                    string shortPattern = pattern.Length > 60 ? pattern.Substring(0, 60) + "..." : pattern;
                    changes.Add($"{(likelyBetter ? "IMPROVED" : "REVIEW ")} {delta} | {shortPattern} (opts={options})");
                    changes.Add($"  BEFORE: {expectedTree}");
                    changes.Add($"  AFTER:  {currentTree}");
                    changes.Add("");
                }
            }

            // Write report
            string reportPath = Path.Combine(Path.GetTempPath(), "regex_baseline_diff.txt");
            var report = new List<string>
            {
                "=== BASELINE COMPARISON REPORT ===",
                $"Total patterns: {total}",
                $"Matched baseline: {matched}",
                $"Changed: {changed} (likely improved: {improved}, needs review: {regressed})",
                $"No baseline (new patterns): {noBaseline}",
                ""
            };
            report.AddRange(changes);
            File.WriteAllLines(reportPath, report);

            _output.WriteLine($"Total: {total}, Matched: {matched}, Changed: {changed}");
            _output.WriteLine($"  Improved: {improved}, Needs review: {regressed}");
            _output.WriteLine($"Report: {reportPath}");

            foreach (string line in changes.Take(100))
            {
                _output.WriteLine(line);
            }
            if (changes.Count > 100)
            {
                _output.WriteLine($"... and {changes.Count - 100} more lines");
            }
        }

        private static int CountSubstring(string text, string sub)
        {
            int count = 0, idx = 0;
            while ((idx = text.IndexOf(sub, idx, StringComparison.Ordinal)) >= 0) { count++; idx += sub.Length; }
            return count;
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
                            root.ReReduceTreeForTests();
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
        /// EXPERIMENT 3: Minimal fix analysis.
        /// Tests what the minimal change to the optimizer would look like:
        ///   (a) Just re-reduce after FinalOptimize (no re-FinalOptimize) — captures Concat+Empty, Atomic unwrap
        ///   (b) Just re-FinalOptimize after re-reduce (no second re-reduce) — captures atomic promotions
        ///   (c) Full re-reduce + re-FinalOptimize (same as Experiment 1 round 1)
        ///
        /// This tells us what the cheapest effective change would be.
        /// </summary>
        [Fact]
        public void MinimalFixAnalysisExperiment()
        {
            int totalPatterns = 0, totalParseErrors = 0;
            int changedReduceOnly = 0, changedFinalOnly = 0, changedBoth = 0;
            var findingsReduceOnly = new List<string>();
            var findingsFinalOnly = new List<string>();
            var findingsBothButNotEither = new List<string>(); // Changed by both but not by either alone

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];
                totalPatterns++;

                try
                {
                    // Baseline: standard pipeline
                    RegexTree baseTree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    string baseline = FlattenTree(baseTree.Root.ToString());

                    // Variant A: standard pipeline + re-reduce only
                    RegexTree treeA = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    treeA.Root.ReReduceTreeForTests();
                    string afterReduceOnly = FlattenTree(treeA.Root.ToString());

                    // Variant B: standard pipeline + re-FinalOptimize only (no re-reduce)
                    RegexTree treeB = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    treeB.Root.ReRunFinalOptimizePasses();
                    string afterFinalOnly = FlattenTree(treeB.Root.ToString());

                    // Variant C: standard pipeline + re-reduce + re-FinalOptimize
                    RegexTree treeC = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    treeC.Root.ReReduceTreeForTests();
                    treeC.Root.ReRunFinalOptimizePasses();
                    string afterBoth = FlattenTree(treeC.Root.ToString());

                    bool reduceChanged = afterReduceOnly != baseline;
                    bool finalChanged = afterFinalOnly != baseline;
                    bool bothChanged = afterBoth != baseline;

                    if (reduceChanged) changedReduceOnly++;
                    if (finalChanged) changedFinalOnly++;
                    if (bothChanged) changedBoth++;

                    if (reduceChanged)
                    {
                        findingsReduceOnly.Add($"REDUCE_ONLY|{Escape(pattern)}|{options}|BEFORE: {baseline}|AFTER: {afterReduceOnly}");
                    }
                    if (finalChanged)
                    {
                        findingsFinalOnly.Add($"FINAL_ONLY|{Escape(pattern)}|{options}|BEFORE: {baseline}|AFTER: {afterFinalOnly}");
                    }
                    if (bothChanged && !reduceChanged && !finalChanged)
                    {
                        findingsBothButNotEither.Add($"SYNERGY|{Escape(pattern)}|{options}|BEFORE: {baseline}|AFTER_BOTH: {afterBoth}|AFTER_REDUCE: {afterReduceOnly}|AFTER_FINAL: {afterFinalOnly}");
                    }
                }
                catch (RegexParseException)
                {
                    totalParseErrors++;
                }
            }

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_minimal_fix_results.txt");
            var lines = new List<string>
            {
                "=== MINIMAL FIX ANALYSIS ===",
                $"Total patterns: {totalPatterns}",
                $"Parse errors: {totalParseErrors}",
                $"",
                $"Changed by re-reduce only: {changedReduceOnly}",
                $"Changed by re-FinalOptimize only: {changedFinalOnly}",
                $"Changed by both (re-reduce + re-FinalOptimize): {changedBoth}",
                $"Changed by BOTH but NOT by either alone (synergy): {findingsBothButNotEither.Count}",
                $"",
                $"=== INTERPRETATION ===",
                $"If re-reduce captures most of the {changedBoth} improvements,",
                $"then the minimal fix is just: add root.ReReduceTreeForTests() after FinalOptimize.",
                $"If many patterns need the full re-reduce+re-FinalOptimize, the fix needs",
                $"to include both passes.",
                $"",
                $"=== REDUCE-ONLY FINDINGS ({findingsReduceOnly.Count}) ===",
                ""
            };
            lines.AddRange(findingsReduceOnly);
            lines.Add($"");
            lines.Add($"=== FINAL-ONLY FINDINGS ({findingsFinalOnly.Count}) ===");
            lines.Add($"");
            lines.AddRange(findingsFinalOnly);
            lines.Add($"");
            lines.Add($"=== SYNERGY FINDINGS ({findingsBothButNotEither.Count}) ===");
            lines.Add($"");
            lines.AddRange(findingsBothButNotEither);
            File.WriteAllLines(outputPath, lines);

            _output.WriteLine($"Total patterns: {totalPatterns}");
            _output.WriteLine($"Changed by re-reduce only: {changedReduceOnly}");
            _output.WriteLine($"Changed by re-FinalOptimize only: {changedFinalOnly}");
            _output.WriteLine($"Changed by both: {changedBoth}");
            _output.WriteLine($"Synergy (both but not either): {findingsBothButNotEither.Count}");
            _output.WriteLine($"Results: {outputPath}");
        }
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
                    tree2Parse.Root.ReReduceTreeForTests();
                    tree2Parse.Root.RunFinalOptimizePassesInOrder(new[] { 1, 0 }); // B→A
                    string tree2 = FlattenTree(tree2Parse.Root.ToString());

                    // Tree with re-reduce → A→B (same order as default, but an extra round)
                    RegexTree tree3Parse = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    tree3Parse.Root.ReReduceTreeForTests();
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

        /// <summary>
        /// EXPERIMENT 4: Match-time performance comparison for affected patterns.
        /// For each pattern that changes with re-reduce, generate random matching/non-matching
        /// input and measure match time with the standard tree vs the improved tree.
        ///
        /// Since we can't easily construct a regex from a modified tree, we instead:
        ///   1. Identify patterns whose trees change
        ///   2. Classify the change type (Tier A structural improvement)
        ///   3. For the most interesting patterns, measure construction time overhead
        ///      of the re-reduce pass
        ///
        /// The construction cost is the ONLY cost — the improved tree is strictly better
        /// or equivalent at match time (Tier A improvements).
        /// </summary>
        [Fact]
        public void ConstructionCostExperiment()
        {
            const int iterations = 1;
            var results = new List<string>();
            var sw = new System.Diagnostics.Stopwatch();

            // Use only the first 2000 patterns to stay within test timeout
            var patterns = new List<(string pattern, int options)>();
            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                patterns.Add(((string)data[0], (int)data[1]));
                if (patterns.Count >= 2000) break;
            }

            // Measure: standard parse only
            sw.Restart();
            for (int iter = 0; iter < iterations; iter++)
            {
                foreach (var (pattern, options) in patterns)
                {
                    try
                    {
                        RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    }
                    catch { }
                }
            }
            sw.Stop();
            long standardMs = sw.ElapsedMilliseconds;
            double standardPerPattern = (double)standardMs / (iterations * patterns.Count);

            // Measure: standard parse + re-reduce
            sw.Restart();
            for (int iter = 0; iter < iterations; iter++)
            {
                foreach (var (pattern, options) in patterns)
                {
                    try
                    {
                        RegexTree tree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                        tree.Root.ReReduceTreeForTests();
                    }
                    catch { }
                }
            }
            sw.Stop();
            long withReReduceMs = sw.ElapsedMilliseconds;
            double withReReducePerPattern = (double)withReReduceMs / (iterations * patterns.Count);

            double overheadMs = withReReducePerPattern - standardPerPattern;
            double overheadPercent = standardPerPattern > 0 ? (overheadMs / standardPerPattern) * 100 : 0;

            results.Add("=== CONSTRUCTION COST EXPERIMENT ===");
            results.Add($"Patterns: {patterns.Count}");
            results.Add($"Iterations: {iterations}");
            results.Add($"");
            results.Add($"Standard parse (avg per pattern): {standardPerPattern:F4} ms");
            results.Add($"Parse + ReReduce (avg per pattern): {withReReducePerPattern:F4} ms");
            results.Add($"ReReduce overhead (per pattern): {overheadMs:F4} ms ({overheadPercent:F1}%)");
            results.Add($"");
            results.Add($"Total standard: {standardMs} ms for {iterations} iterations of {patterns.Count} patterns");
            results.Add($"Total with ReReduce: {withReReduceMs} ms for {iterations} iterations of {patterns.Count} patterns");
            results.Add($"");
            results.Add($"NOTE: This is DEBUG build with ToString() in assertions.");
            results.Add($"Release build overhead would be significantly less.");
            results.Add($"For source generator, this cost is paid at compile time (free at runtime).");

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_construction_cost.txt");
            File.WriteAllLines(outputPath, results);

            foreach (string line in results)
            {
                _output.WriteLine(line);
            }
        }

        /// <summary>
        /// EXPERIMENT 5: Detailed change categorization summary.
        /// For each of the 221 affected patterns, classifies the change type
        /// and outputs a structured summary.
        /// </summary>
        [Fact]
        public void DetailedChangeCategorization()
        {
            var categories = new Dictionary<string, List<string>>
            {
                ["prefix_extraction"] = new(), // ReduceAlternation extracts common prefix in atomic context
                ["concat_empty_removal"] = new(), // Concatenate(X, Empty) → X
                ["atomic_unwrap"] = new(), // Atomic(Xloopatomic) → Xloopatomic
                ["loop_coalesce"] = new(), // Adjacent loops merged
                ["atomic_promotion"] = new(), // Non-atomic → atomic
                ["alternate_to_loop"] = new(), // Alternate(X, Empty) in Atomic → Loop?(X)
                ["other"] = new(),
            };

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int options = (int)data[1];

                try
                {
                    RegexTree baseTree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    string baseline = baseTree.Root.ToString();

                    RegexTree modTree = RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture);
                    modTree.Root.ReReduceTreeForTests();
                    string modified = modTree.Root.ToString();

                    if (baseline == modified) continue;

                    // Count node types
                    int baseEmpty = CountOccurrences(baseline, "Empty");
                    int modEmpty = CountOccurrences(modified, "Empty");
                    int baseAtomic = CountOccurrences(baseline, "Atomic\n") + CountOccurrences(baseline, "Atomic ");
                    int modAtomic = CountOccurrences(modified, "Atomic\n") + CountOccurrences(modified, "Atomic ");
                    int baseConcatenate = CountOccurrences(baseline, "Concatenate");
                    int modConcatenate = CountOccurrences(modified, "Concatenate");
                    int baseAlternate = CountOccurrences(baseline, "Alternate");
                    int modAlternate = CountOccurrences(modified, "Alternate");
                    bool hasNewLoop = modified.Contains("Loop") && !baseline.Contains("Loop") ||
                                     CountOccurrences(modified, "Loop") > CountOccurrences(baseline, "Loop");
                    int baseNodeCount = baseline.Split('\n').Length;
                    int modNodeCount = modified.Split('\n').Length;

                    string shortPattern = pattern.Length > 60 ? pattern.Substring(0, 60) + "..." : pattern;
                    string entry = $"{shortPattern} (opts={options}, nodes: {baseNodeCount}→{modNodeCount})";

                    bool categorized = false;

                    if (baseEmpty > modEmpty && baseConcatenate > modConcatenate)
                    {
                        categories["concat_empty_removal"].Add(entry);
                        categorized = true;
                    }
                    if (baseAtomic > modAtomic)
                    {
                        categories["atomic_unwrap"].Add(entry);
                        categorized = true;
                    }
                    if (baseAlternate > modAlternate && hasNewLoop)
                    {
                        categories["alternate_to_loop"].Add(entry);
                        categorized = true;
                    }
                    if (baseAlternate > modAlternate && !hasNewLoop && baseConcatenate <= modConcatenate)
                    {
                        categories["prefix_extraction"].Add(entry);
                        categorized = true;
                    }

                    if (!categorized)
                    {
                        categories["other"].Add(entry);
                    }
                }
                catch { }
            }

            var lines = new List<string> { "=== DETAILED CHANGE CATEGORIZATION ===" };
            int total = 0;
            foreach (var (cat, patterns) in categories)
            {
                lines.Add($"");
                lines.Add($"--- {cat}: {patterns.Count} patterns ---");
                total += patterns.Count;
                foreach (string p in patterns.Take(20))
                {
                    lines.Add($"  {p}");
                }
                if (patterns.Count > 20)
                {
                    lines.Add($"  ... and {patterns.Count - 20} more");
                }
            }
            lines.Insert(1, $"Total categorized: {total}");

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_change_categories.txt");
            File.WriteAllLines(outputPath, lines);

            foreach (string line in lines)
            {
                _output.WriteLine(line);
            }
        }

        /// <summary>
        /// Dumps before/after trees for ALL 231 changed patterns with compact diff summary.
        /// Run with ReReduceTree DISABLED in production code to see the effect.
        /// </summary>
        [Fact]
        public void DumpAllChangedPatterns()
        {
            // Disable ReReduceTree in production to see before/after.
            // Since ReReduceTree is now in production FinalOptimize, the "before" already includes it.
            // So we compare the tree as-is (WITH ReReduceTree) vs the tree from a second parse
            // where we ALSO call ReReduceTreeForTests (double-reduce — should be same).
            // To get the actual before/after, we need ReReduceTree DISABLED in production.
            // This test is designed to run with it DISABLED.

            var results = new List<(string pattern, int opts, string before, string after, string diffSummary)>();

            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())
            {
                string pattern = (string)data[0];
                int opts = (int)data[1];

                try
                {
                    var tree1 = RegexParser.Parse(pattern, (RegexOptions)opts, CultureInfo.InvariantCulture);
                    string before = tree1.Root.ToString();

                    var tree2 = RegexParser.Parse(pattern, (RegexOptions)opts, CultureInfo.InvariantCulture);
                    tree2.Root.ReReduceTreeForTests();
                    string after = tree2.Root.ToString();

                    if (before == after) continue;

                    // Compute a compact diff summary
                    var diffs = new List<string>();
                    int bEmpty = CountOccurrences(before, "Empty");
                    int aEmpty = CountOccurrences(after, "Empty");
                    int bAtomic = CountOccurrences(before, "\n") > 0 ? before.Split('\n').Count(l => l.TrimStart().StartsWith("Atomic")) : 0;
                    int aAtomic = after.Split('\n').Count(l => l.TrimStart().StartsWith("Atomic"));
                    int bAlt = CountOccurrences(before, "Alternate");
                    int aAlt = CountOccurrences(after, "Alternate");
                    int bConcat = CountOccurrences(before, "Concatenate");
                    int aConcat = CountOccurrences(after, "Concatenate");
                    bool newLoop = CountOccurrences(after, "Loop") > CountOccurrences(before, "Loop");
                    int bNodes = before.Split('\n').Length;
                    int aNodes = after.Split('\n').Length;

                    if (bEmpty > aEmpty) diffs.Add($"Empty-{bEmpty - aEmpty}");
                    if (bAtomic > aAtomic) diffs.Add($"Atomic-{bAtomic - aAtomic}");
                    if (aAtomic > bAtomic) diffs.Add($"Atomic+{aAtomic - bAtomic}");
                    if (bAlt > aAlt) diffs.Add($"Alt-{bAlt - aAlt}");
                    if (aAlt > bAlt) diffs.Add($"Alt+{aAlt - bAlt}");
                    if (bConcat > aConcat) diffs.Add($"Concat-{bConcat - aConcat}");
                    if (aConcat > bConcat) diffs.Add($"Concat+{aConcat - bConcat}");
                    if (newLoop) diffs.Add("Loop+");

                    string summary = string.Join(", ", diffs);
                    results.Add((pattern, opts, before, after, summary));
                }
                catch { }
            }

            // Group by diff summary to find distinct categories
            var groups = results.GroupBy(r => r.diffSummary).OrderByDescending(g => g.Count()).ToList();

            var lines = new List<string>();
            lines.Add($"=== {results.Count} CHANGED PATTERNS IN {groups.Count} DISTINCT DIFF SIGNATURES ===\n");

            foreach (var group in groups)
            {
                lines.Add($"--- [{group.Count()} patterns] {group.Key} ---");
                // Show first 3 examples with full trees
                foreach (var (pattern, opts, before, after, _) in group.Take(3))
                {
                    string shortPat = pattern.Length > 80 ? pattern.Substring(0, 77) + "..." : pattern;
                    lines.Add($"  Pattern: {shortPat} (opts={opts})");
                    lines.Add($"  BEFORE:\n{string.Join("\n", before.Split('\n').Select(l => "    " + l))}");
                    lines.Add($"  AFTER:\n{string.Join("\n", after.Split('\n').Select(l => "    " + l))}");
                    lines.Add("");
                }
                // Show remaining as one-liners
                foreach (var (pattern, opts, _, _, _) in group.Skip(3))
                {
                    string shortPat = pattern.Length > 80 ? pattern.Substring(0, 77) + "..." : pattern;
                    lines.Add($"  + {shortPat} (opts={opts})");
                }
                lines.Add("");
            }

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_all_changes.txt");
            File.WriteAllLines(outputPath, lines);

            // Also write a summary to console
            _output.WriteLine($"{results.Count} changed patterns in {groups.Count} distinct signatures:");
            foreach (var g in groups)
            {
                _output.WriteLine($"  [{g.Count()}] {g.Key}");
                _output.WriteLine($"       e.g. {(g.First().pattern.Length > 60 ? g.First().pattern.Substring(0, 57) + "..." : g.First().pattern)} (opts={g.First().opts})");
            }
        }

        private static int CountOccurrences(string text, string search)
        {
            int count = 0, index = 0;
            while ((index = text.IndexOf(search, index, StringComparison.Ordinal)) >= 0)
            {
                count++;
                index += search.Length;
            }
            return count;
        }

        /// <summary>
        /// Verifies candidate InlineData pairs for PatternsReduceIdentically.
        /// Run with ReReduceTree ENABLED to confirm trees match.
        /// </summary>
        [Fact]
        public void VerifyTestPairs()
        {
            var pairs = new (string actual, string expected)[]
            {
                // Mechanism 3: Redundant Atomic removal
                ("ab|a|ac", "ab?"),
                ("ab|a|ac|d", "(?>ab?|d)"),

                // Mechanism 4: Set-to-One simplification after branch dedup
                ("a?b|a??b", "(?>a?(?>b))"),
                ("[ab]?c|[ab]??c", "(?>[ab]?(?>c))"),
            };

            foreach (var (actual, expected) in pairs)
            {
                string actualStr = RegexParser.Parse(actual, RegexOptions.None, CultureInfo.InvariantCulture).Root.ToString();
                string expectedStr = RegexParser.Parse(expected, RegexOptions.None, CultureInfo.InvariantCulture).Root.ToString();

                bool match = actualStr == expectedStr;
                _output.WriteLine($"[{(match ? "PASS" : "FAIL")}] \"{actual}\" vs \"{expected}\"");
                if (!match)
                {
                    _output.WriteLine($"  ACTUAL TREE:\n{string.Join("\n", actualStr.Split('\n').Select(l => "    " + l))}");
                    _output.WriteLine($"  EXPECTED TREE:\n{string.Join("\n", expectedStr.Split('\n').Select(l => "    " + l))}");
                }
                _output.WriteLine("");
            }
        }

        /// <summary>
        /// Empirically finds simple test patterns for each distinct ReReduceTree improvement mechanism.
        /// Run with ReReduceTree DISABLED in production code.
        /// </summary>
        [Fact]
        public void FindTestCandidates()
        {
            var candidates = new (string pattern, RegexOptions opts, string mechanism)[]
            {
                // Mechanism 3: Redundant Atomic removal — Atomic(Xloopatomic) → Xloopatomic
                ("(?:abc|ab|abd|e)", RegexOptions.None, "atomic-removal"),
                ("(?:ab|a|ac|d)", RegexOptions.None, "atomic-removal"),
                ("(?:ab|a|ac)", RegexOptions.None, "atomic-removal"),
                ("(?:ba|b|ca)", RegexOptions.None, "atomic-removal"),
                ("ab|a|ac|d", RegexOptions.None, "atomic-removal"),
                ("ab|a|ac", RegexOptions.None, "atomic-removal"),

                // Mechanism 4: Identical branch dedup after atomic promotion
                ("a?b|a??b", RegexOptions.None, "branch-dedup"),
                ("a*b|a*?b", RegexOptions.None, "branch-dedup"),
                ("a+b|a+?b", RegexOptions.None, "branch-dedup"),
                ("[ab]?c|[ab]??c", RegexOptions.None, "branch-dedup"),
                ("[ab]+c|[ab]+?c", RegexOptions.None, "branch-dedup"),
                (".?b|.??b", RegexOptions.Singleline, "branch-dedup"),
                ("a?bc|a??bc", RegexOptions.None, "branch-dedup"),

                // Mechanism 5: Loop coalescing — Loop(Set) → Setloop after FinalOptimize
                // This arises when FinalOptimize removes a Capture or Concat leaving Loop(Set)
                ("(?:[-=]){2,}", RegexOptions.None, "loop-coalesce"),
                ("(?:[ab]){3,}", RegexOptions.None, "loop-coalesce"),
                ("(?:a){2,}", RegexOptions.None, "loop-coalesce"),
                ("(?:[a-z])+", RegexOptions.None, "loop-coalesce"),

                // Mechanism 6: Further prefix extraction after simplification
                ("abc|abd|ab", RegexOptions.None, "further-prefix"),
                ("ab|ac|a", RegexOptions.None, "further-prefix"),

                // More complex combos
                ("(?:ab|a)c", RegexOptions.None, "combo-empty-prefix"),
                ("(?:abc|ab|a)d", RegexOptions.None, "combo-empty-prefix"),
                ("a(?:bc|b)", RegexOptions.None, "combo-empty-prefix"),
                ("a(?:bc|b|bd)", RegexOptions.None, "combo-empty-prefix"),
            };

            var lines = new List<string>();
            int changedCount = 0;

            foreach (var (pattern, opts, mechanism) in candidates)
            {
                try
                {
                    var tree1 = RegexParser.Parse(pattern, opts, CultureInfo.InvariantCulture);
                    string before = tree1.Root.ToString();

                    var tree2 = RegexParser.Parse(pattern, opts, CultureInfo.InvariantCulture);
                    tree2.Root.ReReduceTreeForTests();
                    string after = tree2.Root.ToString();

                    bool changed = before != after;
                    if (changed) changedCount++;

                    string status = changed ? "CHANGED" : "same";
                    lines.Add($"[{status}] ({mechanism}) {pattern} (opts={(int)opts})");
                    if (changed)
                    {
                        lines.Add($"  BEFORE:\n{string.Join("\n", before.Split('\n').Select(l => "    " + l))}");
                        lines.Add($"  AFTER:\n{string.Join("\n", after.Split('\n').Select(l => "    " + l))}");
                    }
                    lines.Add("");
                }
                catch (Exception ex)
                {
                    lines.Add($"[ERROR] ({mechanism}) {pattern}: {ex.Message}");
                    lines.Add("");
                }
            }

            lines.Insert(0, $"=== {changedCount} of {candidates.Length} candidates changed ===\n");

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_test_candidates.txt");
            File.WriteAllLines(outputPath, lines);
            foreach (string line in lines) _output.WriteLine(line);
        }

        /// <summary>
        /// Cross-reference: test all dotnet/performance benchmark patterns against ReReduceTree.
        /// </summary>
        [Fact]
        public void BenchmarkPatternCrossReference()
        {
            var patterns = new (string pattern, RegexOptions options, string name)[]
            {
                // Perf_Regex_Common
                (@"^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,12}|[0-9]{1,3})(\]?)$", RegexOptions.None, "Email"),
                (@"\b\d{1,2}\/\d{1,2}\/\d{2,4}\b", RegexOptions.None, "Date"),
                (@"(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9])", RegexOptions.None, "IP"),
                (@"[\w]+://[^/\s?#]+[^\s?#]+(?:\?[^\s#]*)?(?:#[^\s]*)?", RegexOptions.None, "URI"),
                (@"tempus|magna|semper", RegexOptions.None, "MultiWord"),
                (@"\w{10,}", RegexOptions.None, "LongWord"),
                (@"\b\w{10,}\b", RegexOptions.None, "LongWordBoundary"),
                (".*(ss)", RegexOptions.None, "Backtrack"),
                (@"[^a]+\.[^z]+", RegexOptions.None, "SingleNodeBacktrack"),
                (@"(^(.*)(\(([0-9]+),([0-9]+)\)): )(error|warning) ([A-Z]+[0-9]+) ?: (.*)", RegexOptions.None, "WarningPattern"),
                (@"[\w\.+-]+@[\w\.-]+\.[\w\.-]+", RegexOptions.None, "Mariomka_Email"),
                // Sherlock
                (@"Sherlock|Street", RegexOptions.None, "Sherlock_Alt2"),
                (@"Sherlock|Holmes|Watson|Irene|Adler|John|Baker", RegexOptions.None, "Sherlock_Alt7"),
                (@"Sherlock|Holmes|Watson|Irene|Adler|John|Baker", RegexOptions.IgnoreCase, "Sherlock_Alt7_CI"),
                (@"Sher[a-z]+|Hol[a-z]+", RegexOptions.None, "Sherlock_SetAlt"),
                (@"Sher[a-z]+|Hol[a-z]+", RegexOptions.IgnoreCase, "Sherlock_SetAlt_CI"),
                (@"Holmes.{0,25}Watson|Watson.{0,25}Holmes", RegexOptions.None, "Sherlock_Proximity"),
                (@"[a-q][^u-z]{13}x", RegexOptions.None, "Sherlock_Complex"),
                (@"[a-zA-Z]+ing", RegexOptions.None, "Sherlock_Ing"),
                (@"\s[a-zA-Z]{0,12}ing\s", RegexOptions.None, "Sherlock_IngBounded"),
                (@"(?m)^Sherlock Holmes|Sherlock Holmes$", RegexOptions.Multiline, "Sherlock_Anchored"),
                (@"\w+\s+Holmes", RegexOptions.None, "Sherlock_WordHolmes"),
                (@"\w+\s+Holmes\s+\w+", RegexOptions.None, "Sherlock_WordHolmesWord"),
                (@"\b\w+n\b", RegexOptions.None, "Sherlock_WordN"),
                (@"[a-zA-Z]+ing", RegexOptions.None, "Sherlock_Ing2"),
                // Leipzig
                ("Huck[a-zA-Z]+|Saw[a-zA-Z]+", RegexOptions.None, "Leipzig_HuckSaw"),
                ("Tom|Sawyer|Huckleberry|Finn", RegexOptions.None, "Leipzig_TomSawyer"),
                ("Tom|Sawyer|Huckleberry|Finn", RegexOptions.IgnoreCase, "Leipzig_TomSawyer_CI"),
                (".{0,2}(Tom|Sawyer|Huckleberry|Finn)", RegexOptions.None, "Leipzig_Prefix02"),
                (".{2,4}(Tom|Sawyer|Huckleberry|Finn)", RegexOptions.None, "Leipzig_Prefix24"),
                ("Tom.{10,25}river|river.{10,25}Tom", RegexOptions.None, "Leipzig_TomRiver"),
                (@"([A-Za-z]awyer|[A-Za-z]inn)\s", RegexOptions.None, "Leipzig_AwyerInn"),
                // Boost
                (@"^([0-9]+)(\-| |$)(.*)$", RegexOptions.None, "Boost_NumLine"),
                (@"(\d{4}[- ]){3}\d{3,4}", RegexOptions.None, "Boost_CC"),
                (@"^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$", RegexOptions.None, "Boost_Email"),
                (@"^[a-zA-Z]{1,2}[0-9][0-9A-Za-z]{0,1} {0,1}[0-9][A-Za-z]{2}$", RegexOptions.None, "Boost_PostCode"),
                (@"^\d{1,2}/\d{1,2}/\d{4}$", RegexOptions.None, "Boost_Date"),
                (@"^[-+]?\d*\.?\d*$", RegexOptions.None, "Boost_Number"),
            };

            int improved = 0;
            var improvedList = new List<string>();
            foreach (var (pattern, options, name) in patterns)
            {
                try
                {
                    RegexTree tree = RegexParser.Parse(pattern, options, CultureInfo.InvariantCulture);
                    string before = tree.Root.ToString();
                    tree.Root.ReReduceTreeForTests();
                    string after = tree.Root.ToString();

                    if (before != after)
                    {
                        improved++;
                        int bNodes = before.Split('\n').Length;
                        int aNodes = after.Split('\n').Length;
                        _output.WriteLine($"IMPROVED: {name} ({pattern}) nodes: {bNodes}->{aNodes}");
                        _output.WriteLine($"  BEFORE: {FlattenTree(before)}");
                        _output.WriteLine($"  AFTER:  {FlattenTree(after)}");
                        _output.WriteLine("");
                        improvedList.Add($"{name}: {pattern} (nodes: {bNodes}->{aNodes})");
                    }
                    else
                    {
                        _output.WriteLine($"  unchanged: {name}");
                    }
                }
                catch (Exception ex)
                {
                    _output.WriteLine($"  ERROR: {name}: {ex.Message}");
                }
            }
            _output.WriteLine($"");
            _output.WriteLine($"Total benchmark patterns: {patterns.Length}");
            _output.WriteLine($"Improved by ReReduce: {improved}");

            string outputPath = Path.Combine(Path.GetTempPath(), "regex_benchmark_crossref.txt");
            var lines = new List<string>
            {
                "=== BENCHMARK PATTERN CROSS-REFERENCE ===",
                $"Total benchmark patterns tested: {patterns.Length}",
                $"Improved by ReReduce: {improved}",
                ""
            };
            lines.AddRange(improvedList);
            File.WriteAllLines(outputPath, lines);
        }
    }
}
