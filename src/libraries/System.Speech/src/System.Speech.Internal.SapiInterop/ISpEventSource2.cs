using System.Runtime.InteropServices;

namespace System.Speech.Internal.SapiInterop
{
	[ComImport]
	[Guid("2373A435-6A4B-429e-A6AC-D4231A61975B")]
	[InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
	internal interface ISpEventSource2 : ISpEventSource, ISpNotifySource
	{
		new void SetNotifySink(ISpNotifySink pNotifySink);

		new void SetNotifyWindowMessage(uint hWnd, uint Msg, IntPtr wParam, IntPtr lParam);

		new void Slot3();

		new void Slot4();

		new void Slot5();

		[PreserveSig]
		new int WaitForNotifyEvent(uint dwMilliseconds);

		new void Slot7();

		new void SetInterest(ulong ullEventInterest, ulong ullQueuedInterest);

		new void GetEvents(uint ulCount, out SPEVENT pEventArray, out uint pulFetched);

		new void Slot10();

		void GetEventsEx(uint ulCount, out SPEVENTEX pEventArray, out uint pulFetched);
	}
}
