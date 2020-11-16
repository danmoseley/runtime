using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.InteropServices;
using System.Speech.Synthesis.TtsEngine;

namespace System.Speech.Internal.Synthesis
{
	internal class TtsProxyCom : ITtsEngineProxy
	{
		private ITtsEngineSsml _comEngine;

		private IntPtr _iSite;

		internal override AlphabetType EngineAlphabet => AlphabetType.Ipa;

		internal TtsProxyCom(ITtsEngineSsml comEngine, IntPtr iSite, int lcid)
			: base(lcid)
		{
			_iSite = iSite;
			_comEngine = comEngine;
		}

		internal override IntPtr GetOutputFormat(IntPtr targetFormat)
		{
			_comEngine.GetOutputFormat((!(targetFormat != IntPtr.Zero)) ? SpeakOutputFormat.Text : SpeakOutputFormat.WaveFormat, targetFormat, out IntPtr waveHeader);
			return waveHeader;
		}

		internal override void AddLexicon(Uri lexicon, string mediaType)
		{
			_comEngine.AddLexicon(lexicon.ToString(), mediaType, _iSite);
		}

		internal override void RemoveLexicon(Uri lexicon)
		{
			_comEngine.RemoveLexicon(lexicon.ToString(), _iSite);
		}

		internal override void Speak(List<TextFragment> frags, byte[] wfx)
		{
			GCHandle gCHandle = GCHandle.Alloc(wfx, GCHandleType.Pinned);
			try
			{
				IntPtr waveHeader = gCHandle.AddrOfPinnedObject();
				Collection<IntPtr> collection = new Collection<IntPtr>();
				IntPtr fragments = TextFragmentInterop.FragmentToPtr(frags, collection);
				try
				{
					_comEngine.Speak(fragments, frags.Count, waveHeader, _iSite);
				}
				finally
				{
					foreach (IntPtr item in collection)
					{
						Marshal.FreeCoTaskMem(item);
					}
				}
			}
			finally
			{
				gCHandle.Free();
			}
		}

		internal override void ReleaseInterface()
		{
			Marshal.ReleaseComObject(_comEngine);
		}

		internal override char[] ConvertPhonemes(char[] phones, AlphabetType alphabet)
		{
			if (alphabet == AlphabetType.Ipa)
			{
				return phones;
			}
			return _alphabetConverter.SapiToIpa(phones);
		}
	}
}
