using System.Runtime.InteropServices;
using System.Speech.Internal.SapiInterop;

namespace System.Speech.Recognition
{
	[ComImport]
	[Guid("06B64F9E-7FDA-11D2-B4F2-00C04F797396")]
	[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
	internal interface IEnumSpObjectTokens
	{
		void Slot1();

		void Slot2();

		void Slot3();

		void Slot4();

		void Item(uint Index, out ISpObjectToken ppToken);

		void GetCount(out uint pCount);
	}
}
