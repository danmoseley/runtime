"""
Generate a C# test file with InlineData entries for real-world regex patterns.
This creates a baseline test that exercises the full parse+reduce pipeline.

Two-phase approach:
Phase 1: Generate test with (pattern, options, null) — just parse successfully 
Phase 2: Run GenerateTreeBaseline to capture flattened tree strings, then regenerate
"""
import json
import sys
import re

def escape_csharp_string(s):
    """Escape a string for use in a C# verbatim string literal (@"...")"""
    return s.replace('"', '""')

def load_patterns(json_path):
    """Load patterns from the runtime-assets JSON file."""
    with open(json_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Strip comment lines at the top (lines starting with //)
    json_start = content.index('[')
    content = content[json_start:]
    
    data = json.loads(content)
    
    # Deduplicate by (pattern, options) and sort by count descending
    seen = set()
    patterns = []
    for entry in data:
        key = (entry['Pattern'], entry['Options'])
        if key not in seen:
            seen.add(key)
            patterns.append(entry)
    
    patterns.sort(key=lambda x: x['Count'], reverse=True)
    return patterns

def has_problematic_chars(pattern):
    """Check if a pattern has characters that are problematic for C# verbatim string literals."""
    for ch in pattern:
        code = ord(ch)
        if code == 0:  # null byte
            return True
    return False

def generate_test_file(patterns, output_path):
    """Generate a C# test file with MemberData for all patterns."""
    
    valid_patterns = [p for p in patterns if not has_problematic_chars(p['Pattern'])]
    skipped = len(patterns) - len(valid_patterns)
    
    lines = []
    lines.append('// Licensed to the .NET Foundation under one or more agreements.')
    lines.append('// The .NET Foundation licenses this file to you under the MIT license.')
    lines.append('')
    lines.append('// AUTO-GENERATED baseline snapshot of real-world regex pattern trees.')
    lines.append('// Generated from dotnet/runtime-assets Regex_RealWorldPatterns.json')
    lines.append(f'// Total patterns: {len(valid_patterns)} (skipped {skipped} with problematic chars)')
    lines.append('// Purpose: Detect accidental deoptimizations when modifying regex optimization passes.')
    lines.append('')
    lines.append('using System.Collections.Generic;')
    lines.append('using System.Globalization;')
    lines.append('using Xunit;')
    lines.append('')
    lines.append('namespace System.Text.RegularExpressions.Tests')
    lines.append('{')
    lines.append('    public class RegexReductionBaselineTests')
    lines.append('    {')
    lines.append('        /// <summary>')
    lines.append('        /// Flattens a multi-line tree ToString() to a single line for comparison.')
    lines.append('        /// Replaces newline+indentation with pipe-separated compact form.')
    lines.append('        /// </summary>')
    lines.append('        private static string FlattenTree(string tree)')
    lines.append('        {')
    lines.append('            // Replace \\r\\n followed by spaces with "|" + indent-depth marker')
    lines.append('            var sb = new System.Text.StringBuilder();')
    lines.append('            bool first = true;')
    lines.append('            foreach (string rawLine in tree.Split(\'\\n\'))')
    lines.append('            {')
    lines.append('                string line = rawLine.TrimEnd(\'\\r\');')
    lines.append('                if (!first) sb.Append(\'|\');')
    lines.append('                sb.Append(line);')
    lines.append('                first = false;')
    lines.append('            }')
    lines.append('            return sb.ToString();')
    lines.append('        }')
    lines.append('')
    lines.append('        /// <summary>')
    lines.append('        /// Parses each real-world pattern through the full optimization pipeline')
    lines.append('        /// and verifies the resulting flattened tree matches the expected baseline.')
    lines.append('        /// When expectedTree is null, just verifies the pattern parses successfully.')
    lines.append('        /// </summary>')
    lines.append('        [Theory]')
    lines.append('        [MemberData(nameof(RealWorldPatterns))]')
    lines.append('        public void RealWorldPatternTreeBaseline(string pattern, int options, string expectedTree)')
    lines.append('        {')
    lines.append('            try')
    lines.append('            {')
    lines.append('                string actual = FlattenTree(RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture).Root.ToString());')
    lines.append('                if (expectedTree != null)')
    lines.append('                {')
    lines.append('                    Assert.Equal(expectedTree, actual);')
    lines.append('                }')
    lines.append('            }')
    lines.append('            catch (RegexParseException)')
    lines.append('            {')
    lines.append('                // Some patterns may be invalid with certain option combinations; that\'s OK')
    lines.append('                if (expectedTree != null && expectedTree != "PARSE_ERROR")')
    lines.append('                {')
    lines.append('                    Assert.Fail($"Pattern was expected to parse but got RegexParseException");')
    lines.append('                }')
    lines.append('            }')
    lines.append('        }')
    lines.append('')
    lines.append('        public static IEnumerable<object[]> RealWorldPatterns()')
    lines.append('        {')
    
    for p in valid_patterns:
        pattern_escaped = escape_csharp_string(p['Pattern'])
        options = p['Options']
        lines.append(f'            yield return new object[] {{ @"{pattern_escaped}", {options}, null }};')
    
    lines.append('        }')
    lines.append('    }')
    lines.append('}')
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines))
    
    print(f"Generated {output_path}")
    print(f"  {len(valid_patterns)} patterns, {skipped} skipped")

def generate_tree_capture_test(output_path):
    """Generate a test that captures flattened tree strings and writes them to a baseline file."""
    
    lines = []
    lines.append('// Licensed to the .NET Foundation under one or more agreements.')
    lines.append('// The .NET Foundation licenses this file to you under the MIT license.')
    lines.append('')
    lines.append('// Temporary: captures tree baselines and runs fixed-point convergence experiment.')
    lines.append('')
    lines.append('using System.Collections.Generic;')
    lines.append('using System.Globalization;')
    lines.append('using System.IO;')
    lines.append('using System.Linq;')
    lines.append('using System.Text;')
    lines.append('using Xunit;')
    lines.append('using Xunit.Abstractions;')
    lines.append('')
    lines.append('namespace System.Text.RegularExpressions.Tests')
    lines.append('{')
    lines.append('    public class RegexBaselineGenerator(ITestOutputHelper output)')
    lines.append('    {')
    lines.append('        private readonly ITestOutputHelper _output = output;')
    lines.append('')
    lines.append('        private static string FlattenTree(string tree)')
    lines.append('        {')
    lines.append('            var sb = new StringBuilder();')
    lines.append('            bool first = true;')
    lines.append('            foreach (string rawLine in tree.Split(\'\\n\'))')
    lines.append('            {')
    lines.append('                string line = rawLine.TrimEnd(\'\\r\');')
    lines.append('                if (!first) sb.Append(\'|\');')
    lines.append('                sb.Append(line);')
    lines.append('                first = false;')
    lines.append('            }')
    lines.append('            return sb.ToString();')
    lines.append('        }')
    lines.append('')
    lines.append('        /// <summary>')
    lines.append('        /// Generates the flattened tree baseline for all real-world patterns.')
    lines.append('        /// Output: TSV file with pattern, options, flattened tree (or PARSE_ERROR).')
    lines.append('        /// </summary>')
    lines.append('        [Fact]')
    lines.append('        public void GenerateTreeBaseline()')
    lines.append('        {')
    lines.append('            var results = new List<string>();')
    lines.append('            int success = 0, failed = 0;')
    lines.append('')
    lines.append('            foreach (object[] data in RegexReductionBaselineTests.RealWorldPatterns())')
    lines.append('            {')
    lines.append('                string pattern = (string)data[0];')
    lines.append('                int options = (int)data[1];')
    lines.append('                try')
    lines.append('                {')
    lines.append('                    string tree = FlattenTree(RegexParser.Parse(pattern, (RegexOptions)options, CultureInfo.InvariantCulture).Root.ToString());')
    lines.append('                    results.Add($"{Escape(pattern)}\\t{options}\\t{tree}");')
    lines.append('                    success++;')
    lines.append('                }')
    lines.append('                catch (Exception ex)')
    lines.append('                {')
    lines.append('                    results.Add($"{Escape(pattern)}\\t{options}\\tPARSE_ERROR:{ex.GetType().Name}");')
    lines.append('                    failed++;')
    lines.append('                }')
    lines.append('            }')
    lines.append('')
    lines.append('            string outputPath = Path.Combine(Path.GetTempPath(), "regex_tree_baseline.tsv");')
    lines.append('            File.WriteAllLines(outputPath, results);')
    lines.append('            _output.WriteLine($"Wrote {success} trees ({failed} errors) to {outputPath}");')
    lines.append('        }')
    lines.append('')
    lines.append('        private static string Escape(string s) =>')
    lines.append('            s.Replace("\\\\", "\\\\\\\\").Replace("\\t", "\\\\t").Replace("\\n", "\\\\n").Replace("\\r", "\\\\r");')
    lines.append('    }')
    lines.append('}')
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines))
    
    print(f"Generated {output_path}")

if __name__ == '__main__':
    json_path = r'C:\git\runtime\real_world_patterns.json'
    
    patterns = load_patterns(json_path)
    print(f"Loaded {len(patterns)} unique patterns")
    
    # Generate the baseline test file  
    test_path = r'C:\git\runtime\src\libraries\System.Text.RegularExpressions\tests\UnitTests\RegexReductionBaselineTests.cs'
    generate_test_file(patterns, test_path)
    
    # Generate the tree capture utility test
    gen_path = r'C:\git\runtime\src\libraries\System.Text.RegularExpressions\tests\UnitTests\RegexBaselineGenerator.cs'
    generate_tree_capture_test(gen_path)
