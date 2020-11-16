using System.Collections.ObjectModel;
using System.Speech.Internal;

namespace System.Speech.Recognition.SrgsGrammar
{
	[Serializable]
	internal class SrgsItemList : Collection<SrgsItem>
	{
		protected override void InsertItem(int index, SrgsItem item)
		{
			Helpers.ThrowIfNull(item, "item");
			base.InsertItem(index, item);
		}
	}
}
