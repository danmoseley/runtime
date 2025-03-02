// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Xunit;

namespace System.Text.RegularExpressions.Tests
{
    public static class RegexAssert
    {
        public static void Equal(string expected, Capture actual)
        {
            Assert.True(expected == actual.Value, $"Expected to capture '{Regex.Escape(expected)}' but got '{Regex.Escape(actual.Value)}'");
            Assert.Equal(expected, actual.ValueSpan.ToString());
        }
    }
}
