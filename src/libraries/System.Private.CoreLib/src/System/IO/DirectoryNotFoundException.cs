// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;

namespace System.IO
{
    /*
     * Thrown when trying to access a directory that doesn't exist on disk.
     * From COM Interop, this exception is thrown for 2 HRESULTS:
     * the Win32 errorcode-as-HRESULT ERROR_PATH_NOT_FOUND (0x80070003)
     * and STG_E_PATHNOTFOUND (0x80030003).
     */
    [Serializable]
    [TypeForwardedFrom("mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089")]
    public class DirectoryNotFoundException : IOException
    {
        public DirectoryNotFoundException()
            : base(SR.Arg_DirectoryNotFoundException)
        {
            HResult = HResults.COR_E_DIRECTORYNOTFOUND;
        }

        public DirectoryNotFoundException(string? message)
            : base(message ?? SR.Arg_DirectoryNotFoundException)
        {
            HResult = HResults.COR_E_DIRECTORYNOTFOUND;
        }

        public DirectoryNotFoundException(string? message, Exception? innerException)
            : base(message ?? SR.Arg_DirectoryNotFoundException, innerException)
        {
            HResult = HResults.COR_E_DIRECTORYNOTFOUND;
        }

        public DirectoryNotFoundException(string? message, string? directoryPath)
            : base(message ?? SR.Arg_DirectoryNotFoundException)
        {
            HResult = HResults.COR_E_DIRECTORYNOTFOUND;
            DirectoryPath = directoryPath;
        }

        public DirectoryNotFoundException(string? message, string? directoryPath, Exception? innerException)
            : base(message ?? SR.Arg_DirectoryNotFoundException, innerException)
        {
            HResult = HResults.COR_E_DIRECTORYNOTFOUND;
            DirectoryPath = directoryPath;
        }

        public string? DirectoryPath { get; }

        public override string Message
        {
            get
            {
                if (base.Message != SR.Arg_DirectoryNotFoundException || string.IsNullOrEmpty(DirectoryPath))
                {
                    return base.Message;
                }
                return SR.Format(SR.IO_PathNotFound_Path, DirectoryPath);
            }
        }

        [Obsolete(Obsoletions.LegacyFormatterImplMessage, DiagnosticId = Obsoletions.LegacyFormatterImplDiagId, UrlFormat = Obsoletions.SharedUrlFormat)]
        [EditorBrowsable(EditorBrowsableState.Never)]
        protected DirectoryNotFoundException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
        }
    }

    // TEST: DirectoryNotFoundException exposes DirectoryPath and Message is correct
    public class DirectoryNotFoundExceptionTests
    {
        [Xunit.Fact]
        public void DirectoryNotFoundException_DirectoryPath_Message()
        {
            var ex = new DirectoryNotFoundException(null, "/tmp/foo");
            Xunit.Assert.Equal("/tmp/foo", ex.DirectoryPath);
            Xunit.Assert.Contains("/tmp/foo", ex.Message);
        }
    }
}
