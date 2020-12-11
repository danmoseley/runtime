// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Microsoft.Win32;
using System.Collections.Generic;
using System.Globalization;
using System.Speech.Internal.Synthesis;

namespace System.Speech.Internal.ObjectTokens
{
	internal static class SAPICategories
	{
		internal const string CurrentUserVoices = "HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Speech\\Voices";

		internal const string Recognizers = "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Speech\\Recognizers";

		internal const string Voices = "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Speech\\Voices";

		internal const string AudioIn = "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Speech\\AudioInput";

		private static readonly string[] asVersionDefault = new string[1]
		{
			"VersionDefault"
		};

		internal static ObjectToken DefaultToken(string category)
		{
			Helpers.ThrowIfEmptyOrNull(category, "category");
			ObjectToken objectToken = null;
			objectToken = DefaultToken("HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Speech\\" + category, "DefaultTokenId");
			if (objectToken == null)
			{
				objectToken = DefaultToken("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Speech\\" + category, "DefaultTokenId");
			}
			return objectToken;
		}

		internal static int DefaultDeviceOut()
		{
			int result = -1;
			using (ObjectTokenCategory objectTokenCategory = ObjectTokenCategory.Create("HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Speech\\AudioOutput"))
			{
				if (objectTokenCategory != null)
				{
					if (objectTokenCategory.TryGetString("DefaultTokenId", out string value))
					{
						int num = value.IndexOf('\\');
						if (num > 0)
						{
							if (num < value.Length)
							{
								using (RegistryDataKey registryDataKey = RegistryDataKey.Create(value.Substring(num + 1), Registry.LocalMachine))
								{
									if (registryDataKey != null)
									{
										return AudioDeviceOut.GetDevicedId(registryDataKey.Name);
									}
									return result;
								}
							}
							return result;
						}
						return result;
					}
					return result;
				}
				return result;
			}
		}

		private static ObjectToken DefaultToken(string category, string defaultTokenIdValueName)
		{
			ObjectToken objectToken = GetPreference(category, defaultTokenIdValueName);
			if (objectToken != null)
			{
				using (ObjectTokenCategory objectTokenCategory = ObjectTokenCategory.Create(category))
				{
					if (objectTokenCategory == null)
					{
						return objectToken;
					}
					if (objectToken != null)
					{
						foreach (ObjectToken item in (IEnumerable<ObjectToken>)objectTokenCategory)
						{
							objectToken = GetHighestTokenVersion(objectToken, item, asVersionDefault);
						}
						return objectToken;
					}
					string[] sAttributes = new string[1]
					{
						string.Format(CultureInfo.InvariantCulture, "{0:x}", new object[1]
						{
							CultureInfo.CurrentUICulture.LCID
						})
					};
					foreach (ObjectToken item2 in (IEnumerable<ObjectToken>)objectTokenCategory)
					{
						if (item2.MatchesAttributes(sAttributes))
						{
							objectToken = item2;
							break;
						}
					}
					if (objectToken == null)
					{
						using (IEnumerator<ObjectToken> enumerator3 = ((IEnumerable<ObjectToken>)objectTokenCategory).GetEnumerator())
						{
							if (enumerator3.MoveNext())
							{
								return enumerator3.Current;
							}
							return objectToken;
						}
					}
					return objectToken;
				}
			}
			return objectToken;
		}

		private static ObjectToken GetPreference(string category, string defaultLocation)
		{
			ObjectToken result = null;
			using (ObjectTokenCategory objectTokenCategory = ObjectTokenCategory.Create(category))
			{
				if (objectTokenCategory != null)
				{
					if (objectTokenCategory.TryGetString(defaultLocation, out string value))
					{
						return objectTokenCategory.OpenToken(value);
					}
					return result;
				}
				return result;
			}
		}

		private static int CompareTokenVersions(ObjectToken token1, ObjectToken token2, out bool pfDidCompare)
		{
			pfDidCompare = false;
			RegistryDataKey registryDataKey = null;
			RegistryDataKey registryDataKey2 = null;
			registryDataKey = token1.Attributes;
			registryDataKey2 = token2.Attributes;
			if (registryDataKey != null)
			{
				registryDataKey.TryGetString("Vendor", out string value);
				registryDataKey.TryGetString("ProductLine", out string value2);
				registryDataKey.TryGetString("Version", out string value3);
				registryDataKey.TryGetString("Language", out string value4);
				if (registryDataKey2 != null)
				{
					registryDataKey2.TryGetString("Vendor", out string value5);
					registryDataKey2.TryGetString("ProductLine", out string value6);
					registryDataKey2.TryGetString("Version", out string value7);
					registryDataKey2.TryGetString("Language", out string value8);
					if (((string.IsNullOrEmpty(value) && string.IsNullOrEmpty(value5)) || (!string.IsNullOrEmpty(value) && !string.IsNullOrEmpty(value5) && value == value5)) && ((string.IsNullOrEmpty(value2) && string.IsNullOrEmpty(value6)) || (!string.IsNullOrEmpty(value2) && !string.IsNullOrEmpty(value6) && value2 == value6)) && ((string.IsNullOrEmpty(value4) && string.IsNullOrEmpty(value8)) || (!string.IsNullOrEmpty(value4) && !string.IsNullOrEmpty(value8) && value4 == value8)))
					{
						pfDidCompare = true;
						return CompareVersions(value3, value7);
					}
					return -1;
				}
				return 1;
			}
			return -1;
		}

		private static int CompareVersions(string sV1, string sV2)
		{
			ushort[] array = new ushort[4];
			ushort[] array2 = new ushort[4];
			bool flag = ParseVersion(sV1, array);
			bool flag2 = ParseVersion(sV2, array2);
			if (!flag && !flag2)
			{
				return 0;
			}
			if (flag && !flag2)
			{
				return 1;
			}
			if (!flag && flag2)
			{
				return -1;
			}
			for (int i = 0; i < 4; i++)
			{
				if (array[i] > array2[i])
				{
					return 1;
				}
				if (array[i] < array2[i])
				{
					return -1;
				}
			}
			return 0;
		}

		private static bool ParseVersion(string s, ushort[] Version)
		{
			bool flag = true;
			ushort num;
			Version[2] = (num = (Version[3] = 0));
			Version[1] = (num = num);
			Version[0] = num;
			if (string.IsNullOrEmpty(s))
			{
				flag = false;
			}
			else
			{
				int num2 = 0;
				for (int i = 0; i < 4; i++)
				{
					if (num2 >= s.Length)
					{
						break;
					}
					int num3 = s.IndexOf('.', num2);
					string s2 = s.Substring(num2, num3);
					if (!ushort.TryParse(s2, out ushort result) || result > 9999)
					{
						flag = false;
						break;
					}
					Version[i] = result;
					num2 = num3 + 1;
				}
				if (flag && num2 != s.Length)
				{
					flag = false;
				}
			}
			return flag;
		}

		private static ObjectToken GetHighestTokenVersion(ObjectToken token, ObjectToken tokenSeed, string[] criterias)
		{
			if (tokenSeed.MatchesAttributes(criterias))
			{
				bool pfDidCompare;
				int num = CompareTokenVersions(tokenSeed, token, out pfDidCompare);
				if (pfDidCompare && num > 0)
				{
					token = tokenSeed;
				}
			}
			return token;
		}
	}
}
