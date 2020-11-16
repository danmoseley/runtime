using System.Collections;
using System.Collections.Generic;

namespace System.Speech.Internal.SrgsCompiler
{
	internal class ArcList : RedBackList
	{
		internal new Arc First => (Arc)base.First;

		internal List<Arc> ToList()
		{
			List<Arc> list = new List<Arc>();
			IEnumerator enumerator = GetEnumerator();
			try
			{
				while (enumerator.MoveNext())
				{
					Arc item = (Arc)enumerator.Current;
					list.Add(item);
				}
				return list;
			}
			finally
			{
				IDisposable disposable = enumerator as IDisposable;
				if (disposable != null)
				{
					disposable.Dispose();
				}
			}
		}

		protected override int CompareTo(object arc1, object arc2)
		{
			return Arc.CompareContentForKey((Arc)arc1, (Arc)arc2);
		}
	}
}
