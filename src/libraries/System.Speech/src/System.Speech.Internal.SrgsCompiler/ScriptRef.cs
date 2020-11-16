using System.Speech.Internal.SrgsParser;

namespace System.Speech.Internal.SrgsCompiler
{
	internal class ScriptRef
	{
		internal string _rule;

		internal string _sMethod;

		internal RuleMethodScript _method;

		internal int _idSymbol;

		internal ScriptRef(string rule, string sMethod, RuleMethodScript method)
		{
			_rule = rule;
			_sMethod = sMethod;
			_method = method;
		}

		internal void Serialize(StringBlob symbols, StreamMarshaler streamBuffer)
		{
			CfgScriptRef cfgScriptRef = default(CfgScriptRef);
			cfgScriptRef._idRule = symbols.Find(_rule);
			cfgScriptRef._method = _method;
			cfgScriptRef._idMethod = _idSymbol;
			streamBuffer.WriteStream(cfgScriptRef);
		}

		internal static string OnInitMethod(ScriptRef[] scriptRefs, string rule)
		{
			if (scriptRefs != null)
			{
				foreach (ScriptRef scriptRef in scriptRefs)
				{
					if (scriptRef._rule == rule && scriptRef._method == RuleMethodScript.onInit)
					{
						return scriptRef._sMethod;
					}
				}
			}
			return null;
		}
	}
}
