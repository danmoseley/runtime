using System.Globalization;
using System.Resources;

namespace System.Speech
{
	internal static class SR
	{
		private static ResourceManager _resourceManager = new ResourceManager("ExceptionStringTable", typeof(SR).Assembly);

		internal static string Get(SRID id, params object[] args)
		{
			string text = _resourceManager.GetString(id.ToString());
			if (string.IsNullOrEmpty(text))
			{
				text = _resourceManager.GetString("Unavailable");
			}
			else if (args != null && args.Length != 0)
			{
				text = string.Format(CultureInfo.InvariantCulture, text, args);
			}
			return text;
		}
	}
}
