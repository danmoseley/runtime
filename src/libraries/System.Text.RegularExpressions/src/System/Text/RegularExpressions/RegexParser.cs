// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Buffers;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;

namespace System.Text.RegularExpressions
{
    internal sealed class PatternIterator
    {
        private readonly string _realPattern;
        //private int _realPos;
        private string _effectivePattern;
        private int _effectivePos;

        public PatternIterator(string pattern)
        {
            _realPattern = _effectivePattern = pattern;
        }

        /// <summary>Fills in a RegexParseException</summary>
        internal RegexParseException MakeException(RegexParseError error, string message) =>
            new RegexParseException(error, _effectivePos, SR.Format(SR.MakeException, _realPattern, _effectivePos, message));

        /// <summary>Returns the current parsing position.</summary>
        internal int Textpos() => _effectivePos;

        /// <summary>Zaps to a specific parsing position.</summary>
        internal void Textto(int pos) => _effectivePos = pos;

        /// <summary>Returns the char at the right of the current parsing position and advances to the right.</summary>
        internal char RightCharMoveRight() => _effectivePattern[_effectivePos++];

        /// <summary>Moves the current position to the right.</summary>
        internal void MoveRight() => _effectivePos++;

        internal void MoveRight(int i) => _effectivePos += i;

        /// <summary>Moves the current parsing position one to the left.</summary>
        internal void MoveLeft() => --_effectivePos;

        /// <summary>Returns the char left of the current parsing position.</summary>
        internal char CharAt(int i) => _effectivePattern[i];

        /// <summary>Returns the char right of the current parsing position.</summary>
        internal char RightChar() => _effectivePattern[_effectivePos];

        /// <summary>Returns the char i chars right of the current parsing position.</summary>
        internal char RightChar(int i) => _effectivePattern[_effectivePos + i];

        /// <summary>Number of characters to the right of the current parsing position.</summary>
        internal int CharsRight() => _effectivePattern.Length - _effectivePos;

        internal string Substring(int i, int len) => _effectivePattern.Substring(i, len);

        internal void Reset()
        {
            _effectivePattern = _realPattern;
            _effectivePos = 0;
        }

    }

    /// <summary>Builds a tree of RegexNodes from a regular expression.</summary>
    internal ref struct RegexParser
    {
        // Implementation notes:
        // It would be nice to get rid of the comment modes, since the
        // ScanBlank() calls are just kind of duct-taped in.

        private const int EscapeMaxBufferSize = 256;
        private const int OptionStackDefaultSize = 32;
        private const int MaxValueDiv10 = int.MaxValue / 10;
        private const int MaxValueMod10 = int.MaxValue % 10;

        private RegexNode? _stack;
        private RegexNode? _group;
        private RegexNode? _alternation;
        private RegexNode? _concatenation;
        private RegexNode? _unit;

        private readonly PatternIterator _it;
        private readonly CultureInfo _culture;
        private RegexCaseBehavior _caseBehavior;
        private bool _hasIgnoreCaseBackreferenceNodes;

        private int _autocap;
        private int _capcount;
        private int _captop;
        private readonly int _capsize;

        private readonly Hashtable _caps;
        private Hashtable? _capnames;

        private int[]? _capnumlist;
        private List<string>? _capnamelist;

        private RegexOptions _options;
        // NOTE: _optionsStack is ValueListBuilder<int> to ensure that
        //       ArrayPool<int>.Shared, not ArrayPool<RegexOptions>.Shared,
        //       will be created if the stackalloc'd capacity is ever exceeded.
        private ValueListBuilder<int> _optionsStack;

        private bool _ignoreNextParen; // flag to skip capturing a parentheses group

        private RegexParser(string pattern, RegexOptions options, CultureInfo culture, Hashtable caps, int capsize, Hashtable? capnames, Span<int> optionSpan)
        {
            Debug.Assert(pattern != null, "Pattern must be set");
            Debug.Assert(culture != null, "Culture must be set");

            _it = new PatternIterator(pattern);
            _options = options;
            _culture = culture;
            _caseBehavior = default;
            _hasIgnoreCaseBackreferenceNodes = false;
            _caps = caps;
            _capsize = capsize;
            _capnames = capnames;

            _optionsStack = new ValueListBuilder<int>(optionSpan);
            _stack = null;
            _group = null;
            _alternation = null;
            _concatenation = null;
            _unit = null;
            _autocap = 0;
            _capcount = 0;
            _captop = 0;
            _capnumlist = null;
            _capnamelist = null;
            _ignoreNextParen = false;
        }

        /// <summary>Gets the culture to use based on the specified options.</summary>
        internal static CultureInfo GetTargetCulture(RegexOptions options) =>
#pragma warning disable RS1035 // The symbol 'CultureInfo.CurrentCulture' is banned for use by analyzers.
            (options & RegexOptions.CultureInvariant) != 0 ? CultureInfo.InvariantCulture : CultureInfo.CurrentCulture;
#pragma warning restore RS1035

        public static RegexOptions ParseOptionsInPattern(string pattern, RegexOptions options)
        {
            using var parser = new RegexParser(pattern, options, CultureInfo.InvariantCulture, // since we won't perform case conversions, culture doesn't matter in this case.
                new Hashtable(), 0, null, stackalloc int[OptionStackDefaultSize]);

            // We don't really need to Count the Captures, but this method will already do a quick
            // pass through the pattern, and will scan the options found and return them as an out
            // parameter, so we use that to get out the pattern inline options.
            parser.CountCaptures(out RegexOptions foundOptionsInPattern);
            parser.Reset(options);
            return foundOptionsInPattern;
        }

        public static RegexTree Parse(string pattern, RegexOptions options, CultureInfo culture)
        {
            using var parser = new RegexParser(pattern, options, culture, new Hashtable(), 0, null, stackalloc int[OptionStackDefaultSize]);

            parser.CountCaptures(out _);
            parser.Reset(options);
            RegexNode root = parser.ScanRegex();

            int[]? captureNumberList = parser._capnumlist;
            Hashtable? sparseMapping = parser._caps;
            int captop = parser._captop;

            int captureCount;
            if (captureNumberList == null || captop == captureNumberList.Length)
            {
                // The capture list isn't sparse.  Null out the capture mapping as it's not necessary,
                // and store the number of captures.
                captureCount = captop;
                sparseMapping = null;
            }
            else
            {
                // The capture list is sparse.  Store the number of captures, and populate the number-to-names-list.
                captureCount = captureNumberList.Length;
                for (int i = 0; i < captureNumberList.Length; i++)
                {
                    sparseMapping[captureNumberList[i]] = i;
                }
            }

            return new RegexTree(root, captureCount, parser._capnamelist?.ToArray(), parser._capnames!, sparseMapping, options, parser._hasIgnoreCaseBackreferenceNodes ? culture : null);
        }

        /// <summary>
        /// This static call constructs a flat concatenation node given a replacement pattern.
        /// </summary>
        public static RegexReplacement ParseReplacement(string pattern, RegexOptions options, Hashtable caps, int capsize, Hashtable capnames)
        {
            CultureInfo culture = (options & RegexOptions.CultureInvariant) != 0 ? CultureInfo.InvariantCulture : CultureInfo.CurrentCulture;
            using var parser = new RegexParser(pattern, options, culture, caps, capsize, capnames, stackalloc int[OptionStackDefaultSize]);

            RegexNode root = parser.ScanReplacement();
            var regexReplacement = new RegexReplacement(pattern, root, caps);

            return regexReplacement;
        }

        /// <summary>
        /// Escapes all metacharacters (including |,(,),[,{,|,^,$,*,+,?,\, spaces and #)
        /// </summary>
        public static string Escape(string input)
        {
            int indexOfMetachar = IndexOfMetachar(input.AsSpan());
            return indexOfMetachar < 0
                ? input
                : EscapeImpl(input.AsSpan(), indexOfMetachar);
        }

        private static string EscapeImpl(ReadOnlySpan<char> input, int indexOfMetachar)
        {
            // For small inputs we allocate on the stack. In most cases a buffer three
            // times larger the original string should be sufficient as usually not all
            // characters need to be encoded.
            // For larger string we rent the input string's length plus a fixed
            // conservative amount of chars from the ArrayPool.
            ValueStringBuilder vsb = input.Length <= (EscapeMaxBufferSize / 3) ?
                new ValueStringBuilder(stackalloc char[EscapeMaxBufferSize]) :
                new ValueStringBuilder(input.Length + 200);

            while (true)
            {
                vsb.Append(input.Slice(0, indexOfMetachar));
                input = input.Slice(indexOfMetachar);

                if (input.IsEmpty)
                {
                    break;
                }

                char ch = input[0];

                switch (ch)
                {
                    case '\n':
                        ch = 'n';
                        break;
                    case '\r':
                        ch = 'r';
                        break;
                    case '\t':
                        ch = 't';
                        break;
                    case '\f':
                        ch = 'f';
                        break;
                }

                vsb.Append('\\');
                vsb.Append(ch);
                input = input.Slice(1);

                indexOfMetachar = IndexOfMetachar(input);
                if (indexOfMetachar < 0)
                {
                    indexOfMetachar = input.Length;
                }
            }

            return vsb.ToString();
        }

        /// <summary> Unescapes all metacharacters (including (,),[,],{,},|,^,$,*,+,?,\, spaces and #)</summary>
        public static string Unescape(string input)
        {
            int i = input.IndexOf('\\');
            return i >= 0 ?
                UnescapeImpl(input, i) :
                input;
        }

        private static string UnescapeImpl(string input, int i)
        {
            var parser = new RegexParser(input, RegexOptions.None, CultureInfo.InvariantCulture, new Hashtable(), 0, null, stackalloc int[OptionStackDefaultSize]);

            // In the worst case the escaped string has the same length.
            // For small inputs we use stack allocation.
            ValueStringBuilder vsb = input.Length <= EscapeMaxBufferSize ?
                new ValueStringBuilder(stackalloc char[EscapeMaxBufferSize]) :
                new ValueStringBuilder(input.Length);

            vsb.Append(input.AsSpan(0, i));
            do
            {
                i++;
                parser._it.Textto(i);
                if (i < input.Length)
                {
                    vsb.Append(parser.ScanCharEscape());
                }

                i = parser._it.Textpos();
                int lastpos = i;
                while (i < input.Length && input[i] != '\\')
                {
                    i++;
                }

                vsb.Append(input.AsSpan(lastpos, i - lastpos));
            } while (i < input.Length);

            return vsb.ToString();
        }

        /// <summary>Resets parsing to the beginning of the pattern</summary>
        private void Reset(RegexOptions options)
        {
            _it.Reset();
            _autocap = 1;
            _ignoreNextParen = false;
            _optionsStack.Length = 0;
            _options = options;
            _stack = null;
        }

        public void Dispose() => _optionsStack.Dispose();

        /// <summary>The main parsing function</summary>
        private RegexNode ScanRegex()
        {
            char ch;
            bool isQuantifier = false;

            // For the main Capture object, strip out the IgnoreCase option. The rest of the nodes will strip it out depending on the content
            // of each node.
            StartGroup(new RegexNode(RegexNodeKind.Capture, (_options & ~RegexOptions.IgnoreCase), 0, -1));

            while (_it.CharsRight() > 0)
            {
                bool wasPrevQuantifier = isQuantifier;
                isQuantifier = false;

                ScanBlank();

                int startpos = _it.Textpos();

                // move past all of the normal characters.  We'll stop when we hit some kind of control character,
                // or if IgnorePatternWhiteSpace is on, we'll stop when we see some whitespace.
                if (UseOptionX())
                {
                    while (_it.CharsRight() > 0 && (!IsStopperX(ch = _it.RightChar()) || (ch == '{' && !IsTrueQuantifier())))
                        _it.MoveRight();
                }
                else
                {
                    while (_it.CharsRight() > 0 && (!IsSpecial(ch = _it.RightChar()) || (ch == '{' && !IsTrueQuantifier())))
                        _it.MoveRight();
                }

                int endpos = _it.Textpos();

                ScanBlank();

                if (_it.CharsRight() == 0)
                {
                    ch = '!'; // nonspecial, means at end
                }
                else if (IsSpecial(ch = _it.RightChar()))
                {
                    isQuantifier = IsQuantifier(ch);
                    _it.MoveRight();
                }
                else
                {
                    ch = ' '; // nonspecial, means at ordinary char
                }

                if (startpos < endpos)
                {
                    int cchUnquantified = endpos - startpos - (isQuantifier ? 1 : 0);

                    wasPrevQuantifier = false;

                    if (cchUnquantified > 0)
                    {
                        AddConcatenate(startpos, cchUnquantified, false);
                    }

                    if (isQuantifier)
                    {
                        _unit = RegexNode.CreateOneWithCaseConversion(_it.CharAt(endpos - 1), _options, _culture, ref _caseBehavior);
                    }
                }

                switch (ch)
                {
                    case '!':
                        goto BreakOuterScan;

                    case ' ':
                        goto ContinueOuterScan;

                    case '[':
                        {
                            string setString = ScanCharClass(UseOptionI(), scanOnly: false)!.ToStringClass();
                            _unit = new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase, setString);
                        }
                        break;

                    case '(':
                        PushOptions();
                        if (ScanGroupOpen() is RegexNode grouper)
                        {
                            PushGroup();
                            StartGroup(grouper);
                        }
                        else
                        {
                            PopKeepOptions();
                        }
                        continue;

                    case '|':
                        AddAlternate();
                        goto ContinueOuterScan;

                    case ')':
                        if (EmptyStack())
                        {
                            throw _it.MakeException(RegexParseError.InsufficientOpeningParentheses, SR.InsufficientOpeningParentheses);
                        }

                        AddGroup();
                        PopGroup();
                        PopOptions();

                        if (_unit == null)
                        {
                            goto ContinueOuterScan;
                        }
                        break;

                    case '\\':
                        if (_it.CharsRight() == 0)
                        {
                            throw _it.MakeException(RegexParseError.UnescapedEndingBackslash, SR.UnescapedEndingBackslash);
                        }

                        _unit = ScanBackslash(scanOnly: false)!;
                        break;

                    case '^':
                        _unit = GetNodeForCaret();
                        break;

                    case '$':
                        _unit = GetNodeForDollar();
                        break;

                    case '.':
                        _unit = GetNodeForWildcard();
                        break;

                    case '{':
                    case '*':
                    case '+':
                    case '?':
                        if (_unit == null)
                        {
                            throw wasPrevQuantifier ?
                                _it.MakeException(RegexParseError.NestedQuantifiersNotParenthesized, SR.Format(SR.NestedQuantifiersNotParenthesized, ch)) :
                                _it.MakeException(RegexParseError.QuantifierAfterNothing, SR.Format(SR.QuantifierAfterNothing, ch));
                        }
                        _it.MoveLeft();
                        break;

                    default:
                        Debug.Fail($"Unexpected char {ch}");
                        break;
                }

                ScanBlank();

                if (_it.CharsRight() == 0 || !(isQuantifier = IsTrueQuantifier()))
                {
                    AddConcatenate();
                    goto ContinueOuterScan;
                }

                ch = _it.RightCharMoveRight();

                // Handle quantifiers
                while (_unit != null)
                {
                    int min = 0, max = 0;

                    switch (ch)
                    {
                        case '*':
                            max = int.MaxValue;
                            break;

                        case '?':
                            max = 1;
                            break;

                        case '+':
                            min = 1;
                            max = int.MaxValue;
                            break;

                        case '{':
                            startpos = _it.Textpos();
                            max = min = ScanDecimal();
                            if (startpos < _it.Textpos())
                            {
                                if (_it.CharsRight() > 0 && _it.RightChar() == ',')
                                {
                                    _it.MoveRight();
                                    max = _it.CharsRight() == 0 || _it.RightChar() == '}' ? int.MaxValue : ScanDecimal();
                                }
                            }

                            if (startpos == _it.Textpos() || _it.CharsRight() == 0 || _it.RightCharMoveRight() != '}')
                            {
                                AddConcatenate();
                                _it.Textto(startpos - 1);
                                goto ContinueOuterScan;
                            }

                            break;

                        default:
                            Debug.Fail($"Unexpected char {ch}");
                            break;
                    }

                    ScanBlank();

                    bool lazy = false;
                    if (_it.CharsRight() != 0 && _it.RightChar() == '?')
                    {
                        _it.MoveRight();
                        lazy = true;
                    }

                    if (min > max)
                    {
                        throw _it.MakeException(RegexParseError.ReversedQuantifierRange, SR.ReversedQuantifierRange);
                    }

                    AddConcatenate(lazy, min, max);
                }

            ContinueOuterScan:
                ;
            }

        BreakOuterScan:
            ;

            if (!EmptyStack())
            {
                throw _it.MakeException(RegexParseError.InsufficientClosingParentheses, SR.InsufficientClosingParentheses);
            }

            AddGroup();

            return _unit!.FinalOptimize();
        }

        private RegexNode GetNodeForCaret()
        {
            return new RegexNode(UseOptionM() ? RegexNodeKind.Bol : RegexNodeKind.Beginning, _options);
        }

        private RegexNode GetNodeForDollar()
        {
            return new RegexNode(UseOptionM() ? RegexNodeKind.Eol : RegexNodeKind.EndZ, _options);
        }

        private RegexNode GetNodeForWildcard()
        {
            return UseOptionS() ? new RegexNode(RegexNodeKind.Set, _options & ~RegexOptions.IgnoreCase, RegexCharClass.AnyClass) :
                    new RegexNode(RegexNodeKind.Notone, _options & ~RegexOptions.IgnoreCase, '\n');
        }

        private RegexNode GetNodeForBigZ()
        {
            return new RegexNode(RegexNodeKind.EndZ, _options);
        }
        /*
         * Simple parsing for replacement patterns
         */
        private RegexNode ScanReplacement()
        {
            _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);

            while (true)
            {
                int c = _it.CharsRight();
                if (c == 0)
                {
                    break;
                }

                int startpos = _it.Textpos();

                while (c > 0 && _it.RightChar() != '$')
                {
                    _it.MoveRight();
                    c--;
                }

                AddConcatenate(startpos, _it.Textpos() - startpos, true);

                if (c > 0)
                {
                    if (_it.RightCharMoveRight() == '$')
                    {
                        RegexNode node = ScanDollar();
                        _unit = node;
                    }

                    AddConcatenate();
                }
            }

            return _concatenation;
        }

        /*
         * Scans contents of [] (not including []'s), and converts to a
         * RegexCharClass.
         */
        private RegexCharClass? ScanCharClass(bool caseInsensitive, bool scanOnly)
        {
            char ch;
            char chPrev = '\0';
            bool inRange = false;
            bool firstChar = true;
            bool closed = false;

            RegexCharClass? charClass = scanOnly ? null : new RegexCharClass();

            if (_it.CharsRight() > 0 && _it.RightChar() == '^')
            {
                _it.MoveRight();
                if (!scanOnly)
                {
                    charClass!.Negate = true;
                }
                if ((_options & RegexOptions.ECMAScript) != 0 && _it.RightChar() == ']')
                {
                    firstChar = false;
                }
            }

            for (; _it.CharsRight() > 0; firstChar = false)
            {
                bool translatedChar = false;
                ch = _it.RightCharMoveRight();
                if (ch == ']')
                {
                    if (!firstChar)
                    {
                        closed = true;
                        break;
                    }
                }
                else if (ch == '\\' && _it.CharsRight() > 0)
                {
                    switch (ch = _it.RightCharMoveRight())
                    {
                        case 'D':
                        case 'd':
                            if (!scanOnly)
                            {
                                if (inRange)
                                {
                                    throw _it.MakeException(RegexParseError.ShorthandClassInCharacterRange, SR.Format(SR.ShorthandClassInCharacterRange, ch));
                                }
                                charClass!.AddDigit(UseOptionE(), ch == 'D', _it);
                            }
                            continue;

                        case 'S':
                        case 's':
                            if (!scanOnly)
                            {
                                if (inRange)
                                {
                                    throw _it.MakeException(RegexParseError.ShorthandClassInCharacterRange, SR.Format(SR.ShorthandClassInCharacterRange, ch));
                                }
                                charClass!.AddSpace(UseOptionE(), ch == 'S');
                            }
                            continue;

                        case 'W':
                        case 'w':
                            if (!scanOnly)
                            {
                                if (inRange)
                                {
                                    throw _it.MakeException(RegexParseError.ShorthandClassInCharacterRange, SR.Format(SR.ShorthandClassInCharacterRange, ch));
                                }

                                charClass!.AddWord(UseOptionE(), ch == 'W');
                            }
                            continue;

                        case 'p':
                        case 'P':
                            if (!scanOnly)
                            {
                                if (inRange)
                                {
                                    throw _it.MakeException(RegexParseError.ShorthandClassInCharacterRange, SR.Format(SR.ShorthandClassInCharacterRange, ch));
                                }

                                charClass!.AddCategoryFromName(ParseProperty(), ch != 'p', caseInsensitive, _it);
                            }
                            else
                            {
                                ParseProperty();
                            }
                            continue;

                        case '-':
                            if (!scanOnly)
                            {
                                if (inRange)
                                {
                                    if (chPrev > ch)
                                    {
                                        throw _it.MakeException(RegexParseError.ReversedCharacterRange, SR.ReversedCharacterRange);
                                    }

                                    charClass!.AddRange(chPrev, ch);
                                    inRange = false;
                                    chPrev = '\0';
                                }
                                else
                                {
                                    charClass!.AddRange(ch, ch);
                                }
                            }
                            continue;

                        default:
                            _it.MoveLeft();
                            ch = ScanCharEscape(); // non-literal character
                            translatedChar = true;
                            break; // this break will only break out of the switch
                    }
                }
                else if (ch == '[')
                {
                    // This is code for Posix style properties - [:Ll:] or [:IsTibetan:].
                    // It currently doesn't do anything other than skip the whole thing!
                    if (_it.CharsRight() > 0 && _it.RightChar() == ':' && !inRange)
                    {
                        int savePos = _it.Textpos();

                        _it.MoveRight();
                        if (_it.CharsRight() < 2 || _it.RightCharMoveRight() != ':' || _it.RightCharMoveRight() != ']')
                        {
                            _it.Textto(savePos);
                        }
                    }
                }

                if (inRange)
                {
                    inRange = false;
                    if (!scanOnly)
                    {
                        if (ch == '[' && !translatedChar && !firstChar)
                        {
                            // We thought we were in a range, but we're actually starting a subtraction.
                            // In that case, we'll add chPrev to our char class, skip the opening [, and
                            // scan the new character class recursively.
                            charClass!.AddChar(chPrev);
                            charClass.AddSubtraction(ScanCharClass(caseInsensitive, scanOnly)!);

                            if (_it.CharsRight() > 0 && _it.RightChar() != ']')
                            {
                                throw _it.MakeException(RegexParseError.ExclusionGroupNotLast, SR.ExclusionGroupNotLast);
                            }
                        }
                        else
                        {
                            // a regular range, like a-z
                            if (chPrev > ch)
                            {
                                throw _it.MakeException(RegexParseError.ReversedCharacterRange, SR.ReversedCharacterRange);
                            }
                            charClass!.AddRange(chPrev, ch);
                        }
                    }
                }
                else if (_it.CharsRight() >= 2 && _it.RightChar() == '-' && _it.RightChar(1) != ']')
                {
                    // this could be the start of a range
                    chPrev = ch;
                    inRange = true;
                    _it.MoveRight();
                }
                else if (_it.CharsRight() >= 1 && ch == '-' && !translatedChar && _it.RightChar() == '[' && !firstChar)
                {
                    // we aren't in a range, and now there is a subtraction.  Usually this happens
                    // only when a subtraction follows a range, like [a-z-[b]]
                    _it.MoveRight();
                    RegexCharClass? rcc = ScanCharClass(caseInsensitive, scanOnly);
                    if (!scanOnly)
                    {
                        charClass!.AddSubtraction(rcc!);

                        if (_it.CharsRight() > 0 && _it.RightChar() != ']')
                        {
                            throw _it.MakeException(RegexParseError.ExclusionGroupNotLast, SR.ExclusionGroupNotLast);
                        }
                    }
                }
                else
                {
                    if (!scanOnly)
                    {
                        charClass!.AddRange(ch, ch);
                    }
                }
            }

            if (!closed)
            {
                throw _it.MakeException(RegexParseError.UnterminatedBracket, SR.UnterminatedBracket);
            }

            if (!scanOnly && caseInsensitive)
            {
                charClass!.AddCaseEquivalences(_culture);
            }

            return charClass;
        }

        /*
         * Scans chars following a '(' (not counting the '('), and returns
         * a RegexNode for the type of group scanned, or null if the group
         * simply changed options (?cimsx-cimsx) or was a comment (#...).
         */
        private RegexNode? ScanGroupOpen()
        {
            // just return a RegexNode if we have:
            // 1. "(" followed by nothing
            // 2. "(x" where x != ?
            // 3. "(?)"
            if (_it.CharsRight() == 0 || _it.RightChar() != '?' || (_it.RightChar() == '?' && _it.CharsRight() > 1 && _it.RightChar(1) == ')'))
            {
                if (UseOptionN() || _ignoreNextParen)
                {
                    _ignoreNextParen = false;
                    return new RegexNode(RegexNodeKind.Group, _options);
                }
                else
                {
                    return new RegexNode(RegexNodeKind.Capture, _options, _autocap++, -1);
                }
            }

            _it.MoveRight();

            while (true)
            {
                if (_it.CharsRight() == 0)
                {
                    break;
                }

                RegexNodeKind nodeType;
                char close = '>';
                char ch = _it.RightCharMoveRight();
                switch (ch)
                {
                    case ':':
                        // noncapturing group
                        nodeType = RegexNodeKind.Group;
                        break;

                    case '=':
                        // lookahead assertion
                        _options &= ~RegexOptions.RightToLeft;
                        nodeType = RegexNodeKind.PositiveLookaround;
                        break;

                    case '!':
                        // negative lookahead assertion
                        _options &= ~RegexOptions.RightToLeft;
                        nodeType = RegexNodeKind.NegativeLookaround;
                        break;

                    case '>':
                        // atomic subexpression
                        nodeType = RegexNodeKind.Atomic;
                        break;

                    case '\'':
                        close = '\'';
                        goto case '<'; // fallthrough

                    case '<':
                        if (_it.CharsRight() == 0)
                        {
                            goto BreakRecognize;
                        }

                        switch (ch = _it.RightCharMoveRight())
                        {
                            case '=':
                                if (close == '\'')
                                {
                                    goto BreakRecognize;
                                }

                                // lookbehind assertion
                                _options |= RegexOptions.RightToLeft;
                                nodeType = RegexNodeKind.PositiveLookaround;
                                break;

                            case '!':
                                if (close == '\'')
                                {
                                    goto BreakRecognize;
                                }

                                // negative lookbehind assertion
                                _options |= RegexOptions.RightToLeft;
                                nodeType = RegexNodeKind.NegativeLookaround;
                                break;

                            default:
                                _it.MoveLeft();
                                int capnum = -1;
                                int uncapnum = -1;
                                bool proceed = false;

                                // grab part before -

                                if ((uint)(ch - '0') <= 9)
                                {
                                    capnum = ScanDecimal();

                                    if (!IsCaptureSlot(capnum))
                                    {
                                        capnum = -1;
                                    }

                                    // check if we have bogus characters after the number
                                    if (_it.CharsRight() > 0 && !(_it.RightChar() == close || _it.RightChar() == '-'))
                                    {
                                        throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                    }

                                    if (capnum == 0)
                                    {
                                        throw _it.MakeException(RegexParseError.CaptureGroupOfZero, SR.CaptureGroupOfZero);
                                    }
                                }
                                else if (RegexCharClass.IsBoundaryWordChar(ch))
                                {
                                    string capname = ScanCapname();

                                    if (IsCaptureName(capname))
                                    {
                                        capnum = CaptureSlotFromName(capname);
                                    }

                                    // check if we have bogus character after the name
                                    if (_it.CharsRight() > 0 && !(_it.RightChar() == close || _it.RightChar() == '-'))
                                    {
                                        throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                    }
                                }
                                else if (ch == '-')
                                {
                                    proceed = true;
                                }
                                else
                                {
                                    // bad group name - starts with something other than a word character and isn't a number
                                    throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                }

                                // grab part after - if any

                                if ((capnum != -1 || proceed) && _it.CharsRight() > 1 && _it.RightChar() == '-')
                                {
                                    _it.MoveRight();
                                    ch = _it.RightChar();

                                    if ((uint)(ch - '0') <= 9)
                                    {
                                        uncapnum = ScanDecimal();

                                        if (!IsCaptureSlot(uncapnum))
                                        {
                                            throw _it.MakeException(RegexParseError.UndefinedNumberedReference, SR.Format(SR.UndefinedNumberedReference, uncapnum));
                                        }

                                        // check if we have bogus characters after the number
                                        if (_it.CharsRight() > 0 && _it.RightChar() != close)
                                        {
                                            throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                        }
                                    }
                                    else if (RegexCharClass.IsBoundaryWordChar(ch))
                                    {
                                        string uncapname = ScanCapname();

                                        if (IsCaptureName(uncapname))
                                        {
                                            uncapnum = CaptureSlotFromName(uncapname);
                                        }
                                        else
                                        {
                                            throw _it.MakeException(RegexParseError.UndefinedNamedReference, SR.Format(SR.UndefinedNamedReference, uncapname));
                                        }

                                        // check if we have bogus character after the name
                                        if (_it.CharsRight() > 0 && _it.RightChar() != close)
                                        {
                                            throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                        }
                                    }
                                    else
                                    {
                                        // bad group name - starts with something other than a word character and isn't a number
                                        throw _it.MakeException(RegexParseError.CaptureGroupNameInvalid, SR.CaptureGroupNameInvalid);
                                    }
                                }

                                // actually make the node

                                if ((capnum != -1 || uncapnum != -1) && _it.CharsRight() > 0 && _it.RightCharMoveRight() == close)
                                {
                                    return new RegexNode(RegexNodeKind.Capture, _options, capnum, uncapnum);
                                }
                                goto BreakRecognize;
                        }
                        break;

                    case '(':
                        // conditional alternation construct (?(...) | )

                        int parenPos = _it.Textpos();
                        if (_it.CharsRight() > 0)
                        {
                            ch = _it.RightChar();

                            // check if the alternation condition is a backref
                            if (ch >= '0' && ch <= '9')
                            {
                                int capnum = ScanDecimal();
                                if (_it.CharsRight() > 0 && _it.RightCharMoveRight() == ')')
                                {
                                    if (IsCaptureSlot(capnum))
                                    {
                                        return new RegexNode(RegexNodeKind.BackreferenceConditional, _options, capnum);
                                    }

                                    throw _it.MakeException(RegexParseError.AlternationHasUndefinedReference, SR.Format(SR.AlternationHasUndefinedReference, capnum.ToString()));
                                }

                                throw _it.MakeException(RegexParseError.AlternationHasMalformedReference, SR.Format(SR.AlternationHasMalformedReference, capnum.ToString()));
                            }
                            else if (RegexCharClass.IsBoundaryWordChar(ch))
                            {
                                string capname = ScanCapname();

                                if (IsCaptureName(capname) && _it.CharsRight() > 0 && _it.RightCharMoveRight() == ')')
                                {
                                    return new RegexNode(RegexNodeKind.BackreferenceConditional, _options, CaptureSlotFromName(capname));
                                }
                            }
                        }
                        // not a backref
                        nodeType = RegexNodeKind.ExpressionConditional;
                        _it.Textto(parenPos - 1);       // jump to the start of the parentheses
                        _ignoreNextParen = true;    // but make sure we don't try to capture the insides

                        int charsRight = _it.CharsRight();
                        if (charsRight >= 3 && _it.RightChar(1) == '?')
                        {
                            char rightchar2 = _it.RightChar(2);

                            // disallow comments in the condition
                            if (rightchar2 == '#')
                            {
                                throw _it.MakeException(RegexParseError.AlternationHasComment, SR.AlternationHasComment);
                            }

                            // disallow named capture group (?<..>..) in the condition
                            if (rightchar2 == '\'')
                            {
                                throw _it.MakeException(RegexParseError.AlternationHasNamedCapture, SR.AlternationHasNamedCapture);
                            }

                            if (charsRight >= 4 && rightchar2 == '<' && _it.RightChar(3) != '!' && _it.RightChar(3) != '=')
                            {
                                throw _it.MakeException(RegexParseError.AlternationHasNamedCapture, SR.AlternationHasNamedCapture);
                            }
                        }

                        break;

                    default:
                        _it.MoveLeft();

                        nodeType = RegexNodeKind.Group;
                        // Disallow options in the children of a testgroup node
                        if (_group!.Kind != RegexNodeKind.ExpressionConditional)
                        {
                            ScanOptions();
                        }

                        if (_it.CharsRight() == 0)
                        {
                            goto BreakRecognize;
                        }

                        if ((ch = _it.RightCharMoveRight()) == ')')
                        {
                            return null;
                        }

                        if (ch != ':')
                        {
                            goto BreakRecognize;
                        }
                        break;
                }

                return new RegexNode(nodeType, _options);
            }

        BreakRecognize:
            ;
            // break Recognize comes here

            throw _it.MakeException(RegexParseError.InvalidGroupingConstruct, SR.InvalidGroupingConstruct);
        }

        /*
         * Scans whitespace or x-mode comments.
         */
        private void ScanBlank()
        {
            if (UseOptionX())
            {
                while (true)
                {
                    while (_it.CharsRight() > 0 && IsSpace(_it.RightChar()))
                    {
                        _it.MoveRight();
                    }

                    if (_it.CharsRight() == 0)
                    {
                        break;
                    }

                    if (_it.RightChar() == '#')
                    {
                        while (_it.CharsRight() > 0 && _it.RightChar() != '\n')
                        {
                            _it.MoveRight();
                        }
                    }
                    else if (_it.CharsRight() >= 3 && _it.RightChar(2) == '#' && _it.RightChar(1) == '?' && _it.RightChar() == '(')
                    {
                        while (_it.CharsRight() > 0 && _it.RightChar() != ')')
                        {
                            _it.MoveRight();
                        }

                        if (_it.CharsRight() == 0)
                        {
                            throw _it.MakeException(RegexParseError.UnterminatedComment, SR.UnterminatedComment);
                        }

                        _it.MoveRight();
                    }
                    else
                    {
                        break;
                    }
                }
            }
            else
            {
                while (true)
                {
                    if (_it.CharsRight() < 3 || _it.RightChar(2) != '#' || _it.RightChar(1) != '?' || _it.RightChar() != '(')
                    {
                        return;
                    }

                    // skip comment (?# ...)
                    while (_it.CharsRight() > 0 && _it.RightChar() != ')')
                    {
                        _it.MoveRight();
                    }

                    if (_it.CharsRight() == 0)
                    {
                        throw _it.MakeException(RegexParseError.UnterminatedComment, SR.UnterminatedComment);
                    }

                    _it.MoveRight();
                }
            }
        }

        /// <summary>
        /// Scans chars following a '\' (not counting the '\'), and returns
        /// a RegexNode for the type of atom scanned.
        /// </summary>
        private RegexNode? ScanBackslash(bool scanOnly)
        {
            Debug.Assert(_it.CharsRight() > 0, "The current reading position must not be at the end of the pattern");

            char ch;
            switch (ch = _it.RightChar())
            {
                case 'b':
                case 'B':
                case 'A':
                case 'G':
                case 'z':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(TypeFromCode(ch), _options);

                case 'Z':
                    _it.MoveRight();
                    return scanOnly ? null :
                       GetNodeForBigZ();

                case 'w':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.ECMAWordClass : RegexCharClass.WordClass);

                case 'W':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.NotECMAWordClass : RegexCharClass.NotWordClass);

                case 's':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.ECMASpaceClass : RegexCharClass.SpaceClass);

                case 'S':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.NotECMASpaceClass : RegexCharClass.NotSpaceClass);

                case 'd':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.ECMADigitClass : RegexCharClass.DigitClass);

                case 'D':
                    _it.MoveRight();
                    return scanOnly ? null :
                        new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), UseOptionE() ? RegexCharClass.NotECMADigitClass : RegexCharClass.NotDigitClass);

                case 'p':
                case 'P':
                    _it.MoveRight();
                    if (scanOnly)
                    {
                        return null;
                    }

                    var cc = new RegexCharClass();
                    cc.AddCategoryFromName(ParseProperty(), ch != 'p', UseOptionI(), _it);
                    if (UseOptionI())
                    {
                        cc.AddCaseEquivalences(_culture);
                    }

                    return new RegexNode(RegexNodeKind.Set, (_options & ~RegexOptions.IgnoreCase), cc.ToStringClass());

                default:
                    RegexNode? result = ScanBasicBackslash(scanOnly);
                    if (result != null && result.Kind == RegexNodeKind.Backreference && (result.Options & RegexOptions.IgnoreCase) != 0)
                    {
                        _hasIgnoreCaseBackreferenceNodes = true;
                    }
                    return result;
            }
        }

        /// <summary>Scans \-style backreferences and character escapes</summary>
        private RegexNode? ScanBasicBackslash(bool scanOnly)
        {
            Debug.Assert(_it.CharsRight() > 0, "The current reading position must not be at the end of the pattern");

            int backpos = _it.Textpos();
            char close = '\0';
            bool angled = false;
            char ch = _it.RightChar();

            // allow \k<foo> instead of \<foo>, which is now deprecated

            if (ch == 'k')
            {
                if (_it.CharsRight() >= 2)
                {
                    _it.MoveRight();
                    ch = _it.RightCharMoveRight();
                    if (ch is '<' or '\'')
                    {
                        angled = true;
                        close = (ch == '\'') ? '\'' : '>';
                    }
                }

                if (!angled || _it.CharsRight() <= 0)
                {
                    throw _it.MakeException(RegexParseError.MalformedNamedReference, SR.MalformedNamedReference);
                }

                ch = _it.RightChar();
            }

            // Note angle without \g

            else if ((ch == '<' || ch == '\'') && _it.CharsRight() > 1)
            {
                angled = true;
                close = (ch == '\'') ? '\'' : '>';
                _it.MoveRight();
                ch = _it.RightChar();
            }

            // Try to parse backreference: \<1>

            if (angled && ch >= '0' && ch <= '9')
            {
                int capnum = ScanDecimal();

                if (_it.CharsRight() > 0 && _it.RightCharMoveRight() == close)
                {
                    return
                        scanOnly ? null :
                        IsCaptureSlot(capnum) ? new RegexNode(RegexNodeKind.Backreference, _options, capnum) :
                        throw _it.MakeException(RegexParseError.UndefinedNumberedReference, SR.Format(SR.UndefinedNumberedReference, capnum.ToString()));
                }
            }

            // Try to parse backreference or octal: \1

            else if (!angled && ch >= '1' && ch <= '9')
            {
                if (UseOptionE())
                {
                    int capnum = -1;
                    int newcapnum = ch - '0';
                    int pos = _it.Textpos() - 1;
                    while (newcapnum <= _captop)
                    {
                        if (IsCaptureSlot(newcapnum) && (_caps == null || (int)_caps[newcapnum]! < pos))
                        {
                            capnum = newcapnum;
                        }

                        _it.MoveRight();
                        if (_it.CharsRight() == 0 || (ch = _it.RightChar()) < '0' || ch > '9')
                        {
                            break;
                        }

                        newcapnum = newcapnum * 10 + (ch - '0');
                    }

                    if (capnum >= 0)
                    {
                        return scanOnly ? null : new RegexNode(RegexNodeKind.Backreference, _options, capnum);
                    }
                }
                else
                {
                    int capnum = ScanDecimal();

                    if (scanOnly)
                    {
                        return null;
                    }

                    if (IsCaptureSlot(capnum))
                    {
                        return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
                    }

                    if (capnum <= 9)
                    {
                        throw _it.MakeException(RegexParseError.UndefinedNumberedReference, SR.Format(SR.UndefinedNumberedReference, capnum.ToString()));
                    }
                }
            }

            // Try to parse backreference: \<foo>

            else if (angled && RegexCharClass.IsBoundaryWordChar(ch))
            {
                string capname = ScanCapname();

                if (_it.CharsRight() > 0 && _it.RightCharMoveRight() == close)
                {
                    return
                        scanOnly ? null :
                        IsCaptureName(capname) ? new RegexNode(RegexNodeKind.Backreference, _options, CaptureSlotFromName(capname)) :
                        throw _it.MakeException(RegexParseError.UndefinedNamedReference, SR.Format(SR.UndefinedNamedReference, capname));
                }
            }

            // Not backreference: must be char code

            _it.Textto(backpos);
            ch = ScanCharEscape();

            return !scanOnly ?
                RegexNode.CreateOneWithCaseConversion(ch, _options, _culture, ref _caseBehavior) :
                null;
        }

        /*
         * Scans $ patterns recognized within replacement patterns
         */
        private RegexNode ScanDollar()
        {
            if (_it.CharsRight() == 0)
            {
                return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);
            }

            char ch = _it.RightChar();
            bool angled;
            int backpos = _it.Textpos();
            int lastEndPos = backpos;

            // Note angle

            if (ch == '{' && _it.CharsRight() > 1)
            {
                angled = true;
                _it.MoveRight();
                ch = _it.RightChar();
            }
            else
            {
                angled = false;
            }

            // Try to parse backreference: \1 or \{1} or \{cap}

            if (ch >= '0' && ch <= '9')
            {
                if (!angled && UseOptionE())
                {
                    int capnum = -1;
                    int newcapnum = ch - '0';
                    _it.MoveRight();
                    if (IsCaptureSlot(newcapnum))
                    {
                        capnum = newcapnum;
                        lastEndPos = _it.Textpos();
                    }

                    while (_it.CharsRight() > 0 && (ch = _it.RightChar()) >= '0' && ch <= '9')
                    {
                        int digit = ch - '0';
                        if (newcapnum > MaxValueDiv10 || (newcapnum == MaxValueDiv10 && digit > MaxValueMod10))
                        {
                            throw _it.MakeException(RegexParseError.QuantifierOrCaptureGroupOutOfRange, SR.QuantifierOrCaptureGroupOutOfRange);
                        }

                        newcapnum = newcapnum * 10 + digit;

                        _it.MoveRight();
                        if (IsCaptureSlot(newcapnum))
                        {
                            capnum = newcapnum;
                            lastEndPos = _it.Textpos();
                        }
                    }
                    _it.Textto(lastEndPos);
                    if (capnum >= 0)
                    {
                        return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
                    }
                }
                else
                {
                    int capnum = ScanDecimal();
                    if (!angled || _it.CharsRight() > 0 && _it.RightCharMoveRight() == '}')
                    {
                        if (IsCaptureSlot(capnum))
                        {
                            return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
                        }
                    }
                }
            }
            else if (angled && RegexCharClass.IsBoundaryWordChar(ch))
            {
                string capname = ScanCapname();
                if (_it.CharsRight() > 0 && _it.RightCharMoveRight() == '}')
                {
                    if (IsCaptureName(capname))
                    {
                        return new RegexNode(RegexNodeKind.Backreference, _options, CaptureSlotFromName(capname));
                    }
                }
            }
            else if (!angled)
            {
                int capnum = 1;

                switch (ch)
                {
                    case '$':
                        _it.MoveRight();
                        return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);

                    case '&':
                        capnum = 0;
                        break;

                    case '`':
                        capnum = RegexReplacement.LeftPortion;
                        break;

                    case '\'':
                        capnum = RegexReplacement.RightPortion;
                        break;

                    case '+':
                        capnum = RegexReplacement.LastGroup;
                        break;

                    case '_':
                        capnum = RegexReplacement.WholeString;
                        break;
                }

                if (capnum != 1)
                {
                    _it.MoveRight();
                    return new RegexNode(RegexNodeKind.Backreference, _options, capnum);
                }
            }

            // unrecognized $: literalize

            _it.Textto(backpos);
            return RegexNode.CreateOneWithCaseConversion('$', _options, _culture, ref _caseBehavior);
        }

        /*
         * Scans a capture name: consumes word chars
         */
        private string ScanCapname()
        {
            int startpos = _it.Textpos();

            while (_it.CharsRight() > 0)
            {
                if (!RegexCharClass.IsBoundaryWordChar(_it.RightCharMoveRight()))
                {
                    _it.MoveLeft();
                    break;
                }
            }

            return _it.Substring(startpos, _it.Textpos() - startpos);
        }


        /*
         * Scans up to three octal digits (stops before exceeding 0377).
         */
        private char ScanOctal()
        {
            // Consume octal chars only up to 3 digits and value 0377
            int c = 3;
            if (c > _it.CharsRight())
            {
                c = _it.CharsRight();
            }

            int d;
            int i;
            for (i = 0; c > 0 && (uint)(d = _it.RightChar() - '0') <= 7; c -= 1)
            {
                _it.MoveRight();
                i = (i * 8) + d;
                if (UseOptionE() && i >= 0x20)
                {
                    break;
                }
            }

            // Octal codes only go up to 255.  Any larger and the behavior that Perl follows
            // is simply to truncate the high bits.
            i &= 0xFF;

            return (char)i;
        }

        /*
         * Scans any number of decimal digits (pegs value at 2^31-1 if too large)
         */
        private int ScanDecimal()
        {
            int i = 0;
            int d;

            while (_it.CharsRight() > 0 && (uint)(d = (char)(_it.RightChar() - '0')) <= 9)
            {
                _it.MoveRight();

                if (i > MaxValueDiv10 || (i == MaxValueDiv10 && d > MaxValueMod10))
                {
                    throw _it.MakeException(RegexParseError.QuantifierOrCaptureGroupOutOfRange, SR.QuantifierOrCaptureGroupOutOfRange);
                }

                i = (i * 10) + d;
            }

            return i;
        }

        /*
         * Scans exactly c hex digits (c=2 for \xFF, c=4 for \uFFFF)
         */
        private char ScanHex(int c)
        {
            int i = 0;
            int d;

            if (_it.CharsRight() >= c)
            {
                for (; c > 0 && ((d = HexDigit(_it.RightCharMoveRight())) >= 0); c -= 1)
                {
                    i = (i * 0x10) + d;
                }
            }

            if (c > 0)
            {
                throw _it.MakeException(RegexParseError.InsufficientOrInvalidHexDigits, SR.InsufficientOrInvalidHexDigits);
            }

            return (char)i;
        }

        /*
         * Returns n <= 0xF for a hex digit.
         */
        private static int HexDigit(char ch)
        {
            int d;

            if ((uint)(d = ch - '0') <= 9)
                return d;

            if ((uint)(d = ch - 'a') <= 5)
                return d + 0xa;

            if ((uint)(d = ch - 'A') <= 5)
                return d + 0xa;

            return -1;
        }

        /*
         * Grabs and converts an ASCII control character
         */
        private char ScanControl()
        {
            if (_it.CharsRight() == 0)
            {
                throw _it.MakeException(RegexParseError.MissingControlCharacter, SR.MissingControlCharacter);
            }

            char ch = _it.RightCharMoveRight();

            // \ca interpreted as \cA

            if ((uint)(ch - 'a') <= 'z' - 'a')
            {
                ch = (char)(ch - ('a' - 'A'));
            }

            if ((ch = (char)(ch - '@')) < ' ')
            {
                return ch;
            }

            throw _it.MakeException(RegexParseError.UnrecognizedControlCharacter, SR.UnrecognizedControlCharacter);
        }

        /// <summary>Scans cimsx-cimsx option string, stops at the first unrecognized char.</summary>
        private void ScanOptions()
        {
            for (bool off = false; _it.CharsRight() > 0; _it.MoveRight())
            {
                char ch = _it.RightChar();

                if (ch == '-')
                {
                    off = true;
                }
                else if (ch == '+')
                {
                    off = false;
                }
                else
                {
                    RegexOptions options = OptionFromCode(ch);
                    if (options == 0)
                    {
                        return;
                    }

                    if (off)
                    {
                        _options &= ~options;
                    }
                    else
                    {
                        _options |= options;
                    }
                }
            }
        }

        /// <summary>Scans \ code for escape codes that map to single Unicode chars.</summary>
        private char ScanCharEscape()
        {
            char ch = _it.RightCharMoveRight();

            if (ch >= '0' && ch <= '7')
            {
                _it.MoveLeft();
                return ScanOctal();
            }

            switch (ch)
            {
                case 'x':
                    return ScanHex(2);
                case 'u':
                    return ScanHex(4);
                case 'a':
                    return '\u0007';
                case 'b':
                    return '\b';
                case 'e':
                    return '\u001B';
                case 'f':
                    return '\f';
                case 'n':
                    return '\n';
                case 'r':
                    return '\r';
                case 't':
                    return '\t';
                case 'v':
                    return '\u000B';
                case 'c':
                    return ScanControl();
                default:
                    if (!UseOptionE() && RegexCharClass.IsBoundaryWordChar(ch))
                    {
                        throw _it.MakeException(RegexParseError.UnrecognizedEscape, SR.Format(SR.UnrecognizedEscape, ch));
                    }
                    return ch;
            }
        }

        /// <summary>Scans X for \p{X} or \P{X}</summary>
        private string ParseProperty()
        {
            if (_it.CharsRight() < 3)
            {
                throw _it.MakeException(RegexParseError.InvalidUnicodePropertyEscape, SR.InvalidUnicodePropertyEscape);
            }

            char ch = _it.RightCharMoveRight();
            if (ch != '{')
            {
                throw _it.MakeException(RegexParseError.MalformedUnicodePropertyEscape, SR.MalformedUnicodePropertyEscape);
            }

            int startpos = _it.Textpos();
            while (_it.CharsRight() > 0)
            {
                ch = _it.RightCharMoveRight();
                if (!(RegexCharClass.IsBoundaryWordChar(ch) || ch == '-'))
                {
                    _it.MoveLeft();
                    break;
                }
            }

            string capname = _it.Substring(startpos, _it.Textpos() - startpos);

            if (_it.CharsRight() == 0 || _it.RightCharMoveRight() != '}')
            {
                throw _it.MakeException(RegexParseError.InvalidUnicodePropertyEscape, SR.InvalidUnicodePropertyEscape);
            }

            return capname;
        }

        /// <summary>Returns the node kind for zero-length assertions with a \ code.</summary>
        private RegexNodeKind TypeFromCode(char ch) =>
            ch switch
            {
                'b' => UseOptionE() ? RegexNodeKind.ECMABoundary : RegexNodeKind.Boundary,
                'B' => UseOptionE() ? RegexNodeKind.NonECMABoundary : RegexNodeKind.NonBoundary,
                'A' => RegexNodeKind.Beginning,
                'G' => RegexNodeKind.Start,
                'z' => RegexNodeKind.End,
                _ => RegexNodeKind.Nothing,
            };

        /// <summary>Returns option bit from single-char (?imnsx) code.</summary>
        private static RegexOptions OptionFromCode(char ch) =>
            (char)(ch | 0x20) switch
            {
                'i' => RegexOptions.IgnoreCase,
                'm' => RegexOptions.Multiline,
                'n' => RegexOptions.ExplicitCapture,
                's' => RegexOptions.Singleline,
                'x' => RegexOptions.IgnorePatternWhitespace,
                _ => RegexOptions.None,
            };

        /// <summary>
        /// A prescanner for deducing the slots used for captures by doing a partial tokenization of the pattern.
        /// </summary>
        private void CountCaptures(out RegexOptions optionsFoundInPattern)
        {
            NoteCaptureSlot(0, 0);
            optionsFoundInPattern = RegexOptions.None;
            _autocap = 1;

            while (_it.CharsRight() > 0)
            {
                int pos = _it.Textpos();
                char ch = _it.RightCharMoveRight();
                switch (ch)
                {
                    case '\\':
                        if (_it.CharsRight() > 0)
                        {
                            ScanBackslash(scanOnly: true);
                        }
                        break;

                    case '#':
                        if (UseOptionX())
                        {
                            _it.MoveLeft();
                            ScanBlank();
                        }
                        break;

                    case '[':
                        ScanCharClass(caseInsensitive: false, scanOnly: true);
                        break;

                    case ')':
                        if (!EmptyOptionsStack())
                        {
                            PopOptions();
                        }
                        break;

                    case '(':
                        if (_it.CharsRight() >= 2 && _it.RightChar(1) == '#' && _it.RightChar() == '?')
                        {
                            // we have a comment (?#
                            _it.MoveLeft();
                            ScanBlank();
                        }
                        else
                        {
                            PushOptions();
                            if (_it.CharsRight() > 0 && _it.RightChar() == '?')
                            {
                                // we have (?...
                                _it.MoveRight();

                                if (_it.CharsRight() > 1 && (_it.RightChar() == '<' || _it.RightChar() == '\''))
                                {
                                    // named group: (?<... or (?'...

                                    _it.MoveRight();
                                    ch = _it.RightChar();

                                    if (ch != '0' && RegexCharClass.IsBoundaryWordChar(ch))
                                    {
                                        if ((uint)(ch - '1') <= '9' - '1')
                                        {
                                            NoteCaptureSlot(ScanDecimal(), pos);
                                        }
                                        else
                                        {
                                            NoteCaptureName(ScanCapname(), pos);
                                        }
                                    }
                                }
                                else
                                {
                                    // (?...

                                    // get the options if it's an option construct (?cimsx-cimsx...)
                                    ScanOptions();
                                    optionsFoundInPattern |= _options;

                                    if (_it.CharsRight() > 0)
                                    {
                                        if (_it.RightChar() == ')')
                                        {
                                            // (?cimsx-cimsx)
                                            _it.MoveRight();
                                            PopKeepOptions();
                                        }
                                        else if (_it.RightChar() == '(')
                                        {
                                            // alternation construct: (?(foo)yes|no)
                                            // ignore the next paren so we don't capture the condition
                                            _ignoreNextParen = true;

                                            // break from here so we don't reset _ignoreNextParen
                                            break;
                                        }
                                    }
                                }
                            }
                            else
                            {
                                // Simple (unnamed) capture group.
                                // Add unnamed parentheses if ExplicitCapture is not set
                                // and the next parentheses is not ignored.
                                if (!UseOptionN() && !_ignoreNextParen)
                                {
                                    NoteCaptureSlot(_autocap++, pos);
                                }
                            }
                        }

                        _ignoreNextParen = false;
                        break;
                }
            }

            AssignNameSlots();
        }

        /// <summary>Notes a used capture slot</summary>
        private void NoteCaptureSlot(int i, int pos)
        {
            object boxedI = i; // workaround to remove a boxed int when adding to the hashtable
            if (!_caps.ContainsKey(boxedI))
            {
                // the rhs of the hashtable isn't used in the parser

                _caps.Add(boxedI, pos);
                _capcount++;

                if (_captop <= i)
                {
                    _captop = i == int.MaxValue ? i : i + 1;
                }
            }
        }

        /// <summary>Notes a used capture slot</summary>
        private void NoteCaptureName(string name, int pos)
        {
            if (_capnames == null)
            {
                _capnames = new Hashtable();
                _capnamelist = new List<string>();
            }

            if (!_capnames.ContainsKey(name))
            {
                _capnames.Add(name, pos);
                _capnamelist!.Add(name);
            }
        }

        /// <summary>Assigns unused slot numbers to the capture names.</summary>
        private void AssignNameSlots()
        {
            if (_capnames != null)
            {
                for (int i = 0; i < _capnamelist!.Count; i++)
                {
                    while (IsCaptureSlot(_autocap))
                    {
                        _autocap++;
                    }

                    string name = _capnamelist[i];
                    int pos = (int)_capnames[name]!;
                    _capnames[name] = _autocap;
                    NoteCaptureSlot(_autocap, pos);

                    _autocap++;
                }
            }

            // if the caps array has at least one gap, construct the list of used slots

            if (_capcount < _captop)
            {
                _capnumlist = new int[_capcount];
                int i = 0;

                // Manual use of IDictionaryEnumerator instead of foreach to avoid DictionaryEntry box allocations.
                IDictionaryEnumerator de = _caps.GetEnumerator();
                while (de.MoveNext())
                {
                    _capnumlist[i++] = (int)de.Key;
                }

                Array.Sort(_capnumlist);
            }

            // merge capsnumlist into capnamelist

            if (_capnames != null || _capnumlist != null)
            {
                List<string>? oldcapnamelist;
                int next;
                int k = 0;

                if (_capnames == null)
                {
                    oldcapnamelist = null;
                    _capnames = new Hashtable();
                    _capnamelist = new List<string>();
                    next = -1;
                }
                else
                {
                    oldcapnamelist = _capnamelist;
                    _capnamelist = new List<string>();
                    next = (int)_capnames[oldcapnamelist![0]]!;
                }

                for (int i = 0; i < _capcount; i++)
                {
                    int j = (_capnumlist == null) ? i : _capnumlist[i];

                    if (next == j)
                    {
                        _capnamelist.Add(oldcapnamelist![k++]);
                        next = (k == oldcapnamelist.Count) ? -1 : (int)_capnames[oldcapnamelist[k]]!;
                    }
                    else
                    {
                        string str = j.ToString(_culture);
                        _capnamelist.Add(str);
                        _capnames[str] = j;
                    }
                }
            }
        }

        /// <summary>Looks up the slot number for a given name.</summary>
        private int CaptureSlotFromName(string capname) => (int)_capnames![capname]!;

        /// <summary>True if the capture slot was noted</summary>
        private bool IsCaptureSlot(int i)
        {
            if (_caps != null)
            {
                return _caps.ContainsKey(i);
            }

            return i >= 0 && i < _capsize;
        }

        /// <summary>
        /// When generating code on a regex that uses a sparse set
        /// of capture slots, we hash them to a dense set of indices
        /// for an array of capture slots. Instead of doing the hash
        /// at match time, it's done at compile time, here.
        /// </summary>
        internal static int MapCaptureNumber(int capnum, Hashtable? caps) =>
            capnum == -1 ? -1 :
            caps != null ? (int)caps[capnum]! :
            capnum;

        /// <summary>Looks up the slot number for a given name</summary>
        private bool IsCaptureName(string capname) => _capnames != null && _capnames.ContainsKey(capname);

        /// <summary>True if N option disabling '(' autocapture is on.</summary>
        private bool UseOptionN() => (_options & RegexOptions.ExplicitCapture) != 0;

        /// <summary>True if I option enabling case-insensitivity is on.</summary>
        private bool UseOptionI() => (_options & RegexOptions.IgnoreCase) != 0;

        /// <summary>True if M option altering meaning of $ and ^ is on.</summary>
        private bool UseOptionM() => (_options & RegexOptions.Multiline) != 0;

        /// <summary>True if S option altering meaning of . is on.</summary>
        private bool UseOptionS() => (_options & RegexOptions.Singleline) != 0;

        /// <summary> True if X option enabling whitespace/comment mode is on.</summary>
        private bool UseOptionX() => (_options & RegexOptions.IgnorePatternWhitespace) != 0;

        /// <summary>True if E option enabling ECMAScript behavior is on.</summary>
        private bool UseOptionE() => (_options & RegexOptions.ECMAScript) != 0;

        private bool UseOptionA() => (_options & RegexOptions.AnyNewLine) != 0;

        /// <summary>For categorizing ASCII characters.</summary>
        private static ReadOnlySpan<byte> Category => new byte[] {
            // 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
               0, 0, 0, 0, 0, 0, 0, 0, 0, X, X, 0, X, X, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            //    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
               X, 0, 0, Z, S, 0, 0, 0, S, S, Q, Q, 0, 0, S, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Q,
            // @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, S, S, 0, S, 0,
            // '  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Q, S, 0, 0, 0};

#if NET8_0_OR_GREATER
        private static readonly SearchValues<char> s_metachars =
            SearchValues.Create("\t\n\f\r #$()*+.?[\\^{|");

        private static int IndexOfMetachar(ReadOnlySpan<char> input) =>
            input.IndexOfAny(s_metachars);
#else
        private static int IndexOfMetachar(ReadOnlySpan<char> input)
        {
            for (int i = 0; i < input.Length; i++)
            {
                if (IsMetachar(input[i]))
                {
                    return i;
                }
            }

            return -1;
        }
#endif

        /// <summary>Returns true for those characters that terminate a string of ordinary chars.</summary>
        private static bool IsSpecial(char ch) => ch <= '|' && Category[ch] >= S;

        /// <summary>Returns true for those characters that terminate a string of ordinary chars.</summary>
        private static bool IsStopperX(char ch) => ch <= '|' && Category[ch] >= X;

        /// <summary>Returns true for those characters that begin a quantifier.</summary>
        private static bool IsQuantifier(char ch) => ch <= '{' && Category[ch] >= Q;

        private bool IsTrueQuantifier()
        {
            Debug.Assert(_it.CharsRight() > 0, "The current reading position must not be at the end of the pattern");

            int startpos = _it.Textpos();
            char ch = _it.CharAt(startpos);
            if (ch != '{')
            {
                return ch <= '{' && Category[ch] >= Q;
            }

            int pos = startpos;
            int nChars = _it.CharsRight();
            while (--nChars > 0 && (uint)((ch = _it.CharAt(++pos)) - '0') <= 9) ;

            if (nChars == 0 || pos - startpos == 1)
            {
                return false;
            }

            if (ch == '}')
            {
                return true;
            }

            if (ch != ',')
            {
                return false;
            }

            while (--nChars > 0 && (uint)((ch = _it.CharAt(++pos)) - '0') <= 9) ;

            return nChars > 0 && ch == '}';
        }

        /// <summary>Add a string to the last concatenate.</summary>
        private void AddConcatenate(int pos, int cch, bool isReplacement)
        {
            switch (cch)
            {
                case 0:
                    return;

                case 1:
                    _concatenation!.AddChild(RegexNode.CreateOneWithCaseConversion(_it.CharAt(pos), isReplacement ? _options & ~RegexOptions.IgnoreCase : _options, _culture, ref _caseBehavior));
                    break;

                case > 1 when !UseOptionI() || isReplacement || !RegexCharClass.ParticipatesInCaseConversion(_it.Substring(pos, cch).AsSpan()):
                    _concatenation!.AddChild(new RegexNode(RegexNodeKind.Multi, _options & ~RegexOptions.IgnoreCase, _it.Substring(pos, cch)));
                    break;

                default:
                    foreach (char c in _it.Substring(pos, cch))
                    {
                        _concatenation!.AddChild(RegexNode.CreateOneWithCaseConversion(c, _options, _culture, ref _caseBehavior));
                    }
                    break;
            }
        }

        /// <summary>Push the parser state (in response to an open paren)</summary>
        private void PushGroup()
        {
            _group!.Parent = _stack;
            _alternation!.Parent = _group;
            _concatenation!.Parent = _alternation;
            _stack = _concatenation;
        }

        /// <summary>Remember the pushed state (in response to a ')')</summary>
        private void PopGroup()
        {
            _concatenation = _stack;
            _alternation = _concatenation!.Parent;
            _group = _alternation!.Parent;
            _stack = _group!.Parent;

            // The first () inside a Testgroup group goes directly to the group
            if (_group.Kind == RegexNodeKind.ExpressionConditional && _group.ChildCount() == 0)
            {
                if (_unit == null)
                {
                    throw _it.MakeException(RegexParseError.AlternationHasMalformedCondition, SR.AlternationHasMalformedCondition);
                }

                _group.AddChild(_unit);
                _unit = null;
            }
        }

        /// <summary>True if the group stack is empty.</summary>
        private bool EmptyStack() => _stack == null;

        /// <summary>Start a new round for the parser state (in response to an open paren or string start)</summary>
        private void StartGroup(RegexNode openGroup)
        {
            _group = openGroup;
            _alternation = new RegexNode(RegexNodeKind.Alternate, _options);
            _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);
        }

        /// <summary>Finish the current concatenation (in response to a |)</summary>
        private void AddAlternate()
        {
            // The | parts inside a Testgroup group go directly to the group

            if (_group!.Kind is RegexNodeKind.ExpressionConditional or RegexNodeKind.BackreferenceConditional)
            {
                _group.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
            }
            else
            {
                _alternation!.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
            }

            _concatenation = new RegexNode(RegexNodeKind.Concatenate, _options);
        }

        /// <summary>Finish the current quantifiable (when a quantifier is not found or is not possible)</summary>
        private void AddConcatenate()
        {
            // The first (| inside a Testgroup group goes directly to the group

            _concatenation!.AddChild(_unit!);
            _unit = null;
        }

        /// <summary>Finish the current quantifiable (when a quantifier is found)</summary>
        private void AddConcatenate(bool lazy, int min, int max)
        {
            _concatenation!.AddChild(_unit!.MakeQuantifier(lazy, min, max));
            _unit = null;
        }

        /// <summary>Finish the current group (in response to a ')' or end)</summary>
        private void AddGroup()
        {
            if (_group!.Kind is RegexNodeKind.ExpressionConditional or RegexNodeKind.BackreferenceConditional)
            {
                _group.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());

                if (_group.Kind == RegexNodeKind.BackreferenceConditional && _group.ChildCount() > 2 || _group.ChildCount() > 3)
                {
                    throw _it.MakeException(RegexParseError.AlternationHasTooManyConditions, SR.AlternationHasTooManyConditions);
                }
            }
            else
            {
                _alternation!.AddChild(_concatenation!.ReverseConcatenationIfRightToLeft());
                _group.AddChild(_alternation);
            }

            _unit = _group;
        }

        /// <summary>Saves options on a stack.</summary>
        private void PushOptions() => _optionsStack.Append((int)_options);

        /// <summary>Recalls options from the stack.</summary>
        private void PopOptions() => _options = (RegexOptions)_optionsStack.Pop();

        /// <summary>True if options stack is empty.</summary>
        private bool EmptyOptionsStack() => _optionsStack.Length == 0;

        /// <summary>Pops the options stack, but keeps the current options unchanged.</summary>
        private void PopKeepOptions() => _optionsStack.Length--;

        /// <summary>Gets group name from its number.</summary>
        internal static string GroupNameFromNumber(Hashtable? caps, string[]? capslist, int capsize, int i)
        {
            if (capslist is null)
            {
                if ((uint)i < (uint)capsize)
                {
                    return ((uint)i).ToString();
                }
            }
            else
            {
                if ((caps is null || caps.TryGetValue(i, out i)) &&
                    (uint)i < (uint)capslist.Length)
                {
                    return capslist[i];
                }
            }

            return string.Empty;
        }
    }
}
