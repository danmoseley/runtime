// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;
using System.Xml;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Speech.Internal.SrgsParser;

namespace System.Speech.Recognition.SrgsGrammar
{
    /// <summary>
    /// Base class for all SRGS object to build XML fragment corresponding to the object.
    /// </summary>
    [Serializable]
    [DebuggerDisplay("SrgsElement Children:[{_items.Count}]")]
    [DebuggerTypeProxy(typeof(SrgsElementDebugDisplay))]
    public abstract class SrgsElement : MarshalByRefObject, IElement
    {
        /// <summary>
        /// TODOC
        /// </summary>
        protected SrgsElement()
        {
        }


        //*******************************************************************
        //
        // Internal Methods
        //
        //*******************************************************************

        #region Internal methods

        // Write the XML fragment describing the object.
        internal abstract void WriteSrgs(XmlWriter writer);

        // Debugger display string.
        internal abstract string DebuggerDisplayString();

        // Validate the SRGS element.
        /// <summary>
        /// Validate each element and recurse through all the children srgs
        /// elements if any.
        /// Any derived class implementing this mehod must call the base class
        /// in order for the children to be processed.
        /// </summary>
        internal virtual void Validate(SrgsGrammar grammar)
        {
            foreach (SrgsElement element in Children)
            {
                // Child validation
                element.Validate(grammar);
            }
        }

        void IElement.PostParse(IElement parent)
        {
        }

        #endregion

        //*******************************************************************
        //
        // Protected Properties
        //
        //*******************************************************************

        #region Protected Properties

        //TODOC Add Documentation
        virtual internal SrgsElement[] Children
        {
            get
            {
                return Array.Empty<SrgsElement>();
            }
        }

        #endregion

        //*******************************************************************
        //
        // Private Types
        //
        //*******************************************************************

        #region Private Types

        // Used by the debbugger display attribute
        internal class SrgsElementDebugDisplay
        {
            public SrgsElementDebugDisplay(SrgsElement element)
            {
                _elements = element.Children;
            }

            [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
            public SrgsElement[] AKeys
            {
                get
                {
                    return _elements;
                }
            }

            private SrgsElement[] _elements;
        }

        #endregion
    }
}