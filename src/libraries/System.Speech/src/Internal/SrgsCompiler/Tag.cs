// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace System.Speech.Internal.SrgsCompiler
{
    /// <summary>
    /// Summary description for SemanticTag.
    /// </summary>
#if DEBUG
    [DebuggerDisplay ("{_be.Symbols.FromOffset (_cfgTag._nameOffset == 0 ? _cfgTag._valueOffset : _cfgTag._nameOffset)}")]
#endif
    internal sealed class Tag : IComparable<Tag>
    {
        //*******************************************************************
        //
        // Constructors
        //
        //*******************************************************************

        #region Constructors

        internal Tag(Tag tag)
        {
            _be = tag._be;
            _cfgTag = tag._cfgTag;
        }

        internal Tag(Backend be, CfgSemanticTag cfgTag)
        {
            _be = be;
            _cfgTag = cfgTag;
        }

        internal Tag(Backend be, CfgGrammar.CfgProperty property)
        {
            _be = be;
            _cfgTag = new CfgSemanticTag(be.Symbols, property);
        }

        #endregion

        //*******************************************************************
        //
        // Internal Methods
        //
        //*******************************************************************

        #region Internal Methods

        #region IComparable<SemanticTag> Interface implementation

        int IComparable<Tag>.CompareTo(Tag tag)
        {
            return (int)_cfgTag.ArcIndex - (int)tag._cfgTag.ArcIndex;
        }

        #endregion

        internal void Serialize(StreamMarshaler streamBuffer)
        {
            streamBuffer.WriteStream(_cfgTag);
        }

        #endregion

        //*******************************************************************
        //
        // Internal Fields
        //
        //*******************************************************************

        #region Internal Fields

        internal CfgSemanticTag _cfgTag;

        internal Backend _be;

        #endregion
    }
}