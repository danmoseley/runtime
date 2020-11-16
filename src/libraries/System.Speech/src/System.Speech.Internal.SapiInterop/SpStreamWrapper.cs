using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace System.Speech.Internal.SapiInterop
{
	internal class SpStreamWrapper : IStream, IDisposable
	{
		private Stream _stream;

		protected long _endOfStreamPosition = -1L;

		internal SpStreamWrapper(Stream stream)
		{
			_stream = stream;
			_endOfStreamPosition = stream.Length;
		}

		public void Dispose()
		{
			_stream.Dispose();
			GC.SuppressFinalize(this);
		}

		public void Read(byte[] pv, int cb, IntPtr pcbRead)
		{
			if (_endOfStreamPosition >= 0 && _stream.Position + cb > _endOfStreamPosition)
			{
				cb = (int)(_endOfStreamPosition - _stream.Position);
			}
			int num = 0;
			try
			{
				num = _stream.Read(pv, 0, cb);
			}
			catch (EndOfStreamException)
			{
				num = 0;
			}
			if (pcbRead != IntPtr.Zero)
			{
				Marshal.WriteIntPtr(pcbRead, new IntPtr(num));
			}
		}

		public void Write(byte[] pv, int cb, IntPtr pcbWritten)
		{
			throw new NotSupportedException();
		}

		public void Seek(long offset, int seekOrigin, IntPtr plibNewPosition)
		{
			_stream.Seek(offset, (SeekOrigin)seekOrigin);
			if (plibNewPosition != IntPtr.Zero)
			{
				Marshal.WriteIntPtr(plibNewPosition, new IntPtr(_stream.Position));
			}
		}

		public void SetSize(long libNewSize)
		{
			throw new NotSupportedException();
		}

		public void CopyTo(IStream pstm, long cb, IntPtr pcbRead, IntPtr pcbWritten)
		{
			throw new NotSupportedException();
		}

		public void Commit(int grfCommitFlags)
		{
			_stream.Flush();
		}

		public void Revert()
		{
			throw new NotSupportedException();
		}

		public void LockRegion(long libOffset, long cb, int dwLockType)
		{
			throw new NotSupportedException();
		}

		public void UnlockRegion(long libOffset, long cb, int dwLockType)
		{
			throw new NotSupportedException();
		}

		public void Stat(out System.Runtime.InteropServices.ComTypes.STATSTG pstatstg, int grfStatFlag)
		{
			pstatstg = default(System.Runtime.InteropServices.ComTypes.STATSTG);
			pstatstg.cbSize = _stream.Length;
		}

		public void Clone(out IStream ppstm)
		{
			throw new NotSupportedException();
		}
	}
}
