using System.Runtime.InteropServices;

namespace System.Speech.Internal.SapiInterop
{
	[ComImport]
	[Guid("F264DA52-E457-4696-B856-A737B717AF79")]
	[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
	internal interface ISpPhraseEx : ISpPhrase
	{
		new void GetPhrase(out IntPtr ppCoMemPhrase);

		new void GetSerializedPhrase(out IntPtr ppCoMemPhrase);

		new void GetText(uint ulStart, uint ulCount, [MarshalAs(UnmanagedType.Bool)] bool fUseTextReplacements, [MarshalAs(UnmanagedType.LPWStr)] out string ppszCoMemText, out byte pbDisplayAttributes);

		new void Discard(uint dwValueTypes);

		void GetXMLResult([MarshalAs(UnmanagedType.LPWStr)] out string ppszCoMemXMLResult, SPXMLRESULTOPTIONS Options);

		void GetXMLErrorInfo(out SPSEMANTICERRORINFO pSemanticErrorInfo);

		void Slot7();
	}
}
