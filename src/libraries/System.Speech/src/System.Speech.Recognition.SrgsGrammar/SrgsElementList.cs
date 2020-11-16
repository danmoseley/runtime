using System.Collections.ObjectModel;
using System.Speech.Internal;

namespace System.Speech.Recognition.SrgsGrammar
{
	[Serializable]
	internal class SrgsElementList : Collection<SrgsElement>
	{
		protected override void InsertItem(int index, SrgsElement element)
		{
			Helpers.ThrowIfNull(element, "element");
			base.InsertItem(index, element);
		}
	}
}
