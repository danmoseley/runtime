// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Threading;

namespace System
{
    internal static partial class SR
    {
        private static readonly object _lock = new object();
        private static List<string>? _currentlyLoading;
        private static int _infinitelyRecursingCount;
        private static bool _resourceManagerInited;

        private static string InternalGetResourceString(string key)
        {
            if (key.Length == 0)
            {
                Debug.Fail("SR::GetResourceString with empty resourceKey.  Bug in caller, or weird recursive loading problem?");
                return key;
            }

            // If System.Private.CoreLib's ResourceManager
            // calls code that itself throws an exception, constructing that
            // exception will presumably require a resource lookup, potentially
            // causing an infinite loop.
            // Vulnerable code potentially includes IO, CultureInfo,
            // parts of the loader, parts of Reflection, assembly load event handlers, etc.
            //
            // Fortunately, this is limited to System.Private.CoreLib's error lookups and is NOT
            // a general problem for all user code using the ResourceManager.
            //
            // The solution is to make sure only one thread at a time can call
            // GetResourceString.  Also, since resource lookups can be
            // reentrant, if the same thread comes into GetResourceString
            // twice looking for the exact same resource name before
            // returning, we're going into an infinite loop and we should
            // return a bogus string.

            bool lockTaken = false;
            try
            {
                Monitor.Enter(_lock, ref lockTaken);

                // Are we recursively looking up the same resource?  Note - our backout code will set
                // the ResourceHelper's currentlyLoading stack to null if an exception occurs.
                if (_currentlyLoading != null && _currentlyLoading.Count > 0 && _currentlyLoading.LastIndexOf(key) >= 0)
                {
                    // We can start infinitely recursing for one resource lookup,
                    // then during our failure reporting, start infinitely recursing again.
                    // avoid that.
                    if (_infinitelyRecursingCount > 0)
                    {
                        return key;
                    }
                    _infinitelyRecursingCount++;

#if SYSTEM_PRIVATE_CORELIB
                    // Note: our infrastructure for reporting this exception will again cause resource lookup.
                    // This is the most direct way of dealing with that problem.
                    string message = $"Infinite recursion while looking up resource '{key}' within {System.CoreLib.Name}. This may " +
                                    "indicate a corrupt or incomplete .NET installation, a bug in certain " +
                                    "extensibility points such as assembly resolve events or CultureInfo names, " +
                                    "or a bug in {System.CoreLib.Name} itself.";
                    Environment.FailFast(message);
#endif
                }

                _currentlyLoading ??= new List<string>();

                // Call class constructors preemptively, so that we cannot get into an infinite
                // loop constructing a TypeInitializationException.  If this were omitted,
                // we could get the Infinite recursion assert above by failing type initialization
                // between the Push and Pop calls below.
                if (!_resourceManagerInited)
                {
                    RuntimeHelpers.RunClassConstructor(typeof(ResourceManager).TypeHandle);
                    RuntimeHelpers.RunClassConstructor(typeof(ResourceReader).TypeHandle);
                    RuntimeHelpers.RunClassConstructor(typeof(RuntimeResourceSet).TypeHandle);
                    RuntimeHelpers.RunClassConstructor(typeof(BinaryReader).TypeHandle);
                    _resourceManagerInited = true;
                }

                _currentlyLoading.Add(key); // Push

                string? s = ResourceManager.GetString(key, null);
                _currentlyLoading.RemoveAt(_currentlyLoading.Count - 1); // Pop

                Debug.Assert(s != null, $"Resource '{key}' could not be found");
                return s ?? key;
            }
            catch
            {
                if (lockTaken)
                {
                    // Backout code - throw away potentially corrupt state
                    s_resourceManager = null;
                    _currentlyLoading = null;
                }
                throw;
            }
            finally
            {
                if (lockTaken)
                {
                    Monitor.Exit(_lock);
                }
            }
        }
    }
}
