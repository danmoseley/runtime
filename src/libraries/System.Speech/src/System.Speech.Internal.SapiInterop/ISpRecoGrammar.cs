using System.Runtime.InteropServices;

namespace System.Speech.Internal.SapiInterop
{
	[ComImport]
	[Guid("2177DB29-7F45-47D0-8554-067E91C80502")]
	[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
	internal interface ISpRecoGrammar : ISpGrammarBuilder
	{
		new void Slot1();

		new void Slot2();

		new void Slot3();

		new void Slot4();

		new void Slot5();

		new void Slot6();

		new void Slot7();

		new void Slot8();

		void Slot9();

		void Slot10();

		void LoadCmdFromFile([MarshalAs(UnmanagedType.LPWStr)] string pszFileName, SPLOADOPTIONS Options);

		void Slot12();

		void Slot13();

		void LoadCmdFromMemory(IntPtr pGrammar, SPLOADOPTIONS Options);

		void Slot15();

		[PreserveSig]
		int SetRuleState([MarshalAs(UnmanagedType.LPWStr)] string pszName, IntPtr pReserved, SPRULESTATE NewState);

		void Slot17();

		void LoadDictation([MarshalAs(UnmanagedType.LPWStr)] string pszTopicName, SPLOADOPTIONS Options);

		void Slot19();

		[PreserveSig]
		int SetDictationState(SPRULESTATE NewState);

		void SetWordSequenceData([MarshalAs(UnmanagedType.LPWStr)] string pText, uint cchText, ref SPTEXTSELECTIONINFO pInfo);

		void SetTextSelection(ref SPTEXTSELECTIONINFO pInfo);

		void Slot23();

		void SetGrammarState(SPGRAMMARSTATE eGrammarState);

		void Slot25();

		void Slot26();
	}
}
