// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Speech.Internal.SrgsParser;

namespace System.Speech.Internal.SrgsCompiler
{
    internal class Subset : ParseElement, ISubset, IElement
    {
        public Subset(ParseElementCollection parent, Backend backend, string text, MatchMode mode)
            : base(parent._rule)
        {
            char[] achTrimChars = Helpers._achTrimChars;
            foreach (char c in achTrimChars)
            {
                if (c != ' ' && text.Contains(c))
                {
                    text = text.Replace(c, ' ');
                }
            }
            parent.AddArc(backend.SubsetTransition(text, mode));
        }

        void IElement.PostParse(IElement parentElement)
        {
        }
    }
}