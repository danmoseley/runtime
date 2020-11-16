using System.Collections.ObjectModel;
using System.Runtime.InteropServices;

namespace System.Speech.Synthesis.TtsEngine
{
	internal struct ProsodyInterop
	{
		internal ProsodyNumber _pitch;

		internal ProsodyNumber _range;

		internal ProsodyNumber _rate;

		internal int _duration;

		internal ProsodyNumber _volume;

		internal IntPtr _contourPoints;

		internal static IntPtr ProsodyToPtr(Prosody prosody, Collection<IntPtr> memoryBlocks)
		{
			if (prosody == null)
			{
				return IntPtr.Zero;
			}
			ProsodyInterop prosodyInterop = default(ProsodyInterop);
			prosodyInterop._pitch = prosody.Pitch;
			prosodyInterop._range = prosody.Range;
			prosodyInterop._rate = prosody.Rate;
			prosodyInterop._duration = prosody.Duration;
			prosodyInterop._volume = prosody.Volume;
			ContourPoint[] contourPoints = prosody.GetContourPoints();
			if (contourPoints != null)
			{
				int num = Marshal.SizeOf((object)contourPoints[0]);
				prosodyInterop._contourPoints = Marshal.AllocCoTaskMem(contourPoints.Length * num);
				memoryBlocks.Add(prosodyInterop._contourPoints);
				for (uint num2 = 0u; num2 < contourPoints.Length; num2++)
				{
					Marshal.StructureToPtr((object)contourPoints[num2], (IntPtr)((long)prosodyInterop._contourPoints + num * num2), fDeleteOld: false);
				}
			}
			else
			{
				prosodyInterop._contourPoints = IntPtr.Zero;
			}
			IntPtr intPtr = Marshal.AllocCoTaskMem(Marshal.SizeOf((object)prosodyInterop));
			memoryBlocks.Add(intPtr);
			Marshal.StructureToPtr((object)prosodyInterop, intPtr, fDeleteOld: false);
			return intPtr;
		}
	}
}
