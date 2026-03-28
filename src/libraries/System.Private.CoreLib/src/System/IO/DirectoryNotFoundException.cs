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
            : this(message)
        {
            DirectoryPath = directoryPath;
        }

        public DirectoryNotFoundException(string? message, string? directoryPath, Exception? innerException)
            : this(message, innerException)
        {
            DirectoryPath = directoryPath;
        }

        public string? DirectoryPath { get; }

        [Obsolete(Obsoletions.LegacyFormatterImplMessage, DiagnosticId = Obsoletions.LegacyFormatterImplDiagId, UrlFormat = Obsoletions.SharedUrlFormat)]
        [EditorBrowsable(EditorBrowsableState.Never)]
        protected DirectoryNotFoundException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
            DirectoryPath = info.GetString("DirectoryNotFound_DirectoryPath");
        }

        [EditorBrowsable(EditorBrowsableState.Never)]
        [Obsolete("This API supports obsolete formatter-based serialization. It should not be called or extended by application code.", DiagnosticId = "SYSLIB0051", UrlFormat = "https://aka.ms/dotnet-warnings/{0}")]
        public override void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            base.GetObjectData(info, context);
            info.AddValue("DirectoryNotFound_DirectoryPath", DirectoryPath, typeof(string));
        }
    }
}
