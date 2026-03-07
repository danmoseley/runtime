"""
Regenerate RegexReductionBaselineTests.cs with actual expected tree strings
populated from the TSV baseline file.
"""
import json
import os
import sys

def escape_csharp_string(s):
    """Escape a string for use in a C# verbatim string literal (@"...")"""
    return s.replace('"', '""')

def escape_csharp_regular_string(s):
    """Escape for a regular C# string literal ("...")"""
    s = s.replace('\\', '\\\\')
    s = s.replace('"', '\\"')
    s = s.replace('\n', '\\n')
    s = s.replace('\r', '\\r')
    s = s.replace('\t', '\\t')
    return s

def unescape_tsv_field(s):
    """Reverse the Escape() method used in GenerateTreeBaseline:
    .Replace("\\", "\\\\").Replace("\t", "\\t").Replace("\n", "\\n").Replace("\r", "\\r")
    
    Must handle \\r (escaped backslash + r) vs \r (escaped CR) correctly.
    Process char-by-char to avoid ambiguity.
    """
    result = []
    i = 0
    while i < len(s):
        if s[i] == '\\' and i + 1 < len(s):
            next_ch = s[i + 1]
            if next_ch == '\\':
                result.append('\\')
                i += 2
            elif next_ch == 'r':
                result.append('\r')
                i += 2
            elif next_ch == 'n':
                result.append('\n')
                i += 2
            elif next_ch == 't':
                result.append('\t')
                i += 2
            else:
                result.append(s[i])
                i += 1
        else:
            result.append(s[i])
            i += 1
    return ''.join(result)

def load_tsv_baselines(tsv_path):
    """Load the TSV baseline file. Each line: escaped_pattern\toptions\tflattened_tree"""
    baselines = {}
    with open(tsv_path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.rstrip('\n').rstrip('\r')
            parts = line.split('\t', 2)
            if len(parts) < 3:
                continue
            escaped_pattern = parts[0]
            options = int(parts[1])
            tree = parts[2]
            
            pattern = unescape_tsv_field(escaped_pattern)
            key = (pattern, options)
            
            if tree.startswith('PARSE_ERROR:'):
                baselines[key] = 'PARSE_ERROR'
            else:
                baselines[key] = tree
    
    return baselines

def load_patterns(json_path):
    """Load patterns from the runtime-assets JSON file."""
    with open(json_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    json_start = content.index('[')
    content = content[json_start:]
    data = json.loads(content)
    
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
    for ch in pattern:
        if ord(ch) == 0:
            return True
    return False

def generate_test_file(patterns, baselines, output_path):
    """Generate a C# test file with expected trees populated."""
    
    valid_patterns = [p for p in patterns if not has_problematic_chars(p['Pattern'])]
    skipped = len(patterns) - len(valid_patterns)
    
    matched = 0
    unmatched = 0
    
    lines = []
    lines.append('// Licensed to the .NET Foundation under one or more agreements.')
    lines.append('// The .NET Foundation licenses this file to you under the MIT license.')
    lines.append('')
    lines.append('// AUTO-GENERATED baseline snapshot of real-world regex pattern trees.')
    lines.append('// Generated from dotnet/runtime-assets Regex_RealWorldPatterns.json')
    lines.append(f'// Total patterns: {len(valid_patterns)} (skipped {skipped} with problematic chars)')
    lines.append('// Purpose: Detect regressions/improvements when modifying regex optimization passes.')
    lines.append('// Expected trees are captured from the CURRENT optimizer output.')
    lines.append('// After making optimizer changes, re-run these tests:')
    lines.append('//   - Failures = patterns whose trees changed (review each one)')
    lines.append('//   - Pass = patterns unaffected by the change')
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
    lines.append('        /// </summary>')
    lines.append('        private static string FlattenTree(string tree)')
    lines.append('        {')
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
        pattern = p['Pattern']
        options = p['Options']
        pattern_escaped = escape_csharp_string(pattern)
        
        key = (pattern, options)
        tree = baselines.get(key)
        
        if tree is not None:
            matched += 1
            # Use verbatim string for the expected tree too
            tree_escaped = escape_csharp_string(tree)
            lines.append(f'            yield return new object[] {{ @"{pattern_escaped}", {options}, @"{tree_escaped}" }};')
        else:
            unmatched += 1
            lines.append(f'            yield return new object[] {{ @"{pattern_escaped}", {options}, null }};')
    
    lines.append('        }')
    lines.append('    }')
    lines.append('}')
    
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(lines))
    
    print(f"Generated {output_path}")
    print(f"  {len(valid_patterns)} patterns ({matched} with expected trees, {unmatched} without)")

if __name__ == '__main__':
    json_path = os.path.join(os.path.dirname(__file__), 'real_world_patterns.json')
    tsv_path = os.path.join(os.environ['TEMP'], 'regex_tree_baseline.tsv')
    output_path = os.path.join(
        os.path.dirname(__file__),
        'src', 'libraries', 'System.Text.RegularExpressions', 'tests', 'UnitTests',
        'RegexReductionBaselineTests.cs'
    )
    
    print(f"Loading patterns from {json_path}")
    patterns = load_patterns(json_path)
    print(f"  {len(patterns)} unique patterns")
    
    print(f"Loading baselines from {tsv_path}")
    baselines = load_tsv_baselines(tsv_path)
    print(f"  {len(baselines)} baseline trees")
    
    generate_test_file(patterns, baselines, output_path)
