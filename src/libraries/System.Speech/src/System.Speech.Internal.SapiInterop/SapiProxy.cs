// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Runtime.InteropServices;
using System.Threading;

namespace System.Speech.Internal.SapiInterop
{
    internal abstract class SapiProxy : IDisposable
    {
        internal class PassThrough : SapiProxy, IDisposable
        {
            internal PassThrough(ISpRecognizer recognizer)
            {
                _recognizer = recognizer;
            }

            ~PassThrough()
            {
                Dispose(disposing: false);
            }

            public override void Dispose()
            {
                try
                {
                    Dispose(disposing: true);
                }
                finally
                {
                    base.Dispose();
                }
            }

            internal override object Invoke(ObjectDelegate pfn)
            {
                return pfn();
            }

            internal override void Invoke2(VoidDelegate pfn)
            {
                pfn();
            }

            private void Dispose(bool disposing)
            {
                _recognizer2 = null;
                _speechRecognizer = null;
                Marshal.ReleaseComObject(_recognizer);
            }
        }

        internal class MTAThread : SapiProxy, IDisposable
        {
            private Thread _mta;

            private AutoResetEvent _process = new AutoResetEvent(initialState: false);

            private AutoResetEvent _done = new AutoResetEvent(initialState: false);

            private ObjectDelegate _doit;

            private VoidDelegate _doit2;

            private object _result;

            private Exception _exception;

            internal MTAThread(SapiRecognizer.RecognizerType type)
            {
                _mta = new Thread(SapiMTAThread);
                if (!_mta.TrySetApartmentState(ApartmentState.MTA))
                {
                    throw new InvalidOperationException();
                }
                _mta.IsBackground = true;
                _mta.Start();
                if (type == SapiRecognizer.RecognizerType.InProc)
                {
                    Invoke2(delegate
                    {
                        _recognizer = (ISpRecognizer)new SpInprocRecognizer();
                    });
                }
                else
                {
                    Invoke2(delegate
                    {
                        _recognizer = (ISpRecognizer)new SpSharedRecognizer();
                    });
                }
            }

            ~MTAThread()
            {
                Dispose(disposing: false);
            }

            public override void Dispose()
            {
                try
                {
                    Dispose(disposing: true);
                }
                finally
                {
                    base.Dispose();
                }
            }

            internal override object Invoke(ObjectDelegate pfn)
            {
                lock (this)
                {
                    _doit = pfn;
                    _process.Set();
                    _done.WaitOne();
                    if (_exception == null)
                    {
                        return _result;
                    }
                    throw _exception;
                }
            }

            internal override void Invoke2(VoidDelegate pfn)
            {
                lock (this)
                {
                    _doit2 = pfn;
                    _process.Set();
                    _done.WaitOne();
                    if (_exception != null)
                    {
                        throw _exception;
                    }
                }
            }

            private void Dispose(bool disposing)
            {
                lock (this)
                {
                    _recognizer2 = null;
                    _speechRecognizer = null;
                    Invoke2(delegate
                    {
                        Marshal.ReleaseComObject(_recognizer);
                    });
                    ((IDisposable)_process).Dispose();
                    ((IDisposable)_done).Dispose();
                }
                base.Dispose();
            }

            private void SapiMTAThread()
            {
                while (true)
                {
                    try
                    {
                        _process.WaitOne();
                        _exception = null;
                        if (_doit != null)
                        {
                            _result = _doit();
                            _doit = null;
                        }
                        else
                        {
                            _doit2();
                            _doit2 = null;
                        }
                    }
                    catch (Exception exception)
                    {
                        Exception ex = _exception = exception;
                    }
                    try
                    {
                        _done.Set();
                    }
                    catch (ObjectDisposedException)
                    {
                        break;
                    }
                }
            }
        }

        internal delegate object ObjectDelegate();

        internal delegate void VoidDelegate();

        protected ISpeechRecognizer _speechRecognizer;

        protected ISpRecognizer2 _recognizer2;

        protected ISpRecognizer _recognizer;

        internal ISpRecognizer Recognizer => _recognizer;

        internal ISpRecognizer2 Recognizer2
        {
            get
            {
                if (_recognizer2 == null)
                {
                    _recognizer2 = (ISpRecognizer2)_recognizer;
                }
                return _recognizer2;
            }
        }

        internal ISpeechRecognizer SapiSpeechRecognizer
        {
            get
            {
                if (_speechRecognizer == null)
                {
                    _speechRecognizer = (ISpeechRecognizer)_recognizer;
                }
                return _speechRecognizer;
            }
        }

        public virtual void Dispose()
        {
            GC.SuppressFinalize(this);
        }

        internal abstract object Invoke(ObjectDelegate pfn);

        internal abstract void Invoke2(VoidDelegate pfn);
    }
}
