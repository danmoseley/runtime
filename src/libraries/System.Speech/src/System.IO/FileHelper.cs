// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Security;

namespace System.IO
{
    internal static class FileHelper
    {
        internal static FileStream CreateAndOpenTemporaryFile(out string filePath, FileAccess fileAccess = FileAccess.Write, FileOptions fileOptions = FileOptions.None, string extension = null, string subFolder = "WPF")
        {
            int num = 5;
            filePath = null;
            string text = Path.GetTempPath();
            if (!string.IsNullOrEmpty(subFolder))
            {
                string text2 = Path.Combine(text, subFolder);
                if (!Directory.Exists(text2))
                {
                    Directory.CreateDirectory(text2);
                }
                text = text2;
            }
            FileStream fileStream = null;
            while (fileStream == null)
            {
                string text3 = Path.Combine(text, Path.GetRandomFileName());
                if (!string.IsNullOrEmpty(extension))
                {
                    text3 = Path.ChangeExtension(text3, extension);
                }
                num--;
                try
                {
                    fileStream = new FileStream(text3, FileMode.CreateNew, fileAccess, FileShare.None, 4096, fileOptions);
                    filePath = text3;
                }
                catch (Exception ex) when (num > 0 && (ex is IOException || ex is UnauthorizedAccessException))
                {
                }
            }
            return fileStream;
        }

        internal static void DeleteTemporaryFile(string filePath)
        {
            if (!string.IsNullOrEmpty(filePath))
            {
                try
                {
                    File.Delete(filePath);
                }
                catch (IOException)
                {
                }
            }
        }
    }
}
