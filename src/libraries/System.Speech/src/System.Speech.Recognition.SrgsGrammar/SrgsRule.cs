using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Speech.Internal;
using System.Speech.Internal.SrgsParser;
using System.Xml;

namespace System.Speech.Recognition.SrgsGrammar
{
	/// <summary>Represents a grammar rule.</summary>
	[Serializable]
	[DebuggerDisplay("Rule={_id.ToString()} Scope={_scope.ToString()}")]
	[DebuggerTypeProxy(typeof(SrgsRuleDebugDisplay))]
	public class SrgsRule : IRule, IElement
	{
		internal class SrgsRuleDebugDisplay
		{
			private SrgsRule _rule;

			public object Id => _rule.Id;

			public object Scope => _rule.Scope;

			public object BaseClass => _rule.BaseClass;

			public object Script => _rule.Script;

			public object OnInit => _rule.OnInit;

			public object OnParse => _rule.OnParse;

			public object OnError => _rule.OnError;

			public object OnRecognition => _rule.OnRecognition;

			public object Count => _rule._elements.Count;

			[DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
			public SrgsElement[] AKeys
			{
				get
				{
					SrgsElement[] array = new SrgsElement[_rule._elements.Count];
					for (int i = 0; i < _rule._elements.Count; i++)
					{
						array[i] = _rule._elements[i];
					}
					return array;
				}
			}

			public SrgsRuleDebugDisplay(SrgsRule rule)
			{
				_rule = rule;
			}
		}

		private SrgsElementList _elements;

		private string _id;

		private SrgsRuleScope _scope = SrgsRuleScope.Private;

		private RuleDynamic _dynamic = RuleDynamic.NotSet;

		private bool _isScopeSet;

		private string _baseclass;

		private string _script = string.Empty;

		private string _onInit;

		private string _onParse;

		private string _onError;

		private string _onRecognition;

		private static readonly char[] invalidChars = new char[23]
		{
			'?',
			'*',
			'+',
			'|',
			'(',
			')',
			'^',
			'$',
			'/',
			';',
			'.',
			'=',
			'<',
			'>',
			'[',
			']',
			'{',
			'}',
			'\\',
			' ',
			'\t',
			'\r',
			'\n'
		};

		/// <summary>Gets the collection of <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsElement" /> objects in the <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsRule" /> instance.</summary>
		/// <returns>The collection of elements in the rule.</returns>
		public Collection<SrgsElement> Elements => _elements;

		/// <summary>Gets or sets the identifier for the rule.</summary>
		/// <returns>The identifier for the rule.</returns>
		/// <exception cref="T:System.FormatException">An attempt is made to set <see cref="P:System.Speech.Recognition.SrgsGrammar.SrgsRule.Id" /> to an invalid value.</exception>
		public string Id
		{
			get
			{
				return _id;
			}
			set
			{
				XmlParser.ValidateRuleId(value);
				_id = value;
			}
		}

		/// <summary>Gets or sets whether a rule can be activated for recognition and when the rule can be referenced by other rules.</summary>
		/// <returns>A value the sets the scope for the rule.</returns>
		public SrgsRuleScope Scope
		{
			get
			{
				return _scope;
			}
			set
			{
				_scope = value;
				_isScopeSet = true;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string BaseClass
		{
			get
			{
				return _baseclass;
			}
			set
			{
				_baseclass = value;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string Script
		{
			get
			{
				return _script;
			}
			set
			{
				Helpers.ThrowIfEmptyOrNull(value, "value");
				_script = value;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string OnInit
		{
			get
			{
				return _onInit;
			}
			set
			{
				ValidateIdentifier(value);
				_onInit = value;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string OnParse
		{
			get
			{
				return _onParse;
			}
			set
			{
				ValidateIdentifier(value);
				_onParse = value;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string OnError
		{
			get
			{
				return _onError;
			}
			set
			{
				ValidateIdentifier(value);
				_onError = value;
			}
		}

		/// <summary>This property is not currently supported.</summary>
		/// <returns>Not supported.</returns>
		public string OnRecognition
		{
			get
			{
				return _onRecognition;
			}
			set
			{
				ValidateIdentifier(value);
				_onRecognition = value;
			}
		}

		internal RuleDynamic Dynamic
		{
			get
			{
				return _dynamic;
			}
			set
			{
				_dynamic = value;
			}
		}

		internal bool HasCode => _script.Length > 0;

		private SrgsRule()
		{
			_elements = new SrgsElementList();
		}

		/// <summary>Initializes a new instance of the <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsRule" /> class and specifies the identifier for the rule.</summary>
		/// <param name="id">The identifier of the rule.</param>
		/// <exception cref="T:System.ArgumentNullException">
		///   <paramref name="id" /> is <see langword="null" />.</exception>
		/// <exception cref="T:System.ArgumentOutOfRangeException">
		///   <paramref name="id" /> is empty.</exception>
		/// <exception cref="T:System.FormatException">
		///   <paramref name="id" /> is not a proper rule identifier.</exception>
		public SrgsRule(string id)
			: this()
		{
			XmlParser.ValidateRuleId(id);
			Id = id;
		}

		/// <summary>Initializes a new instance of the <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsRule" /> class from an array of <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsElement" /> objects.</summary>
		/// <param name="id">The identifier of the rule.</param>
		/// <param name="elements">An array of <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsElement" /> elements.</param>
		/// <exception cref="T:System.ArgumentNullException">
		///   <paramref name="id" /> is <see langword="null" />.  
		/// <paramref name="elements" /> is <see langword="null" />.</exception>
		/// <exception cref="T:System.ArgumentOutOfRangeException">
		///   <paramref name="id" /> is empty.</exception>
		/// <exception cref="T:System.FormatException">
		///   <paramref name="id" /> is not a proper rule identifier.</exception>
		public SrgsRule(string id, params SrgsElement[] elements)
			: this()
		{
			Helpers.ThrowIfNull(elements, "elements");
			XmlParser.ValidateRuleId(id);
			Id = id;
			int num = 0;
			while (true)
			{
				if (num < elements.Length)
				{
					if (elements[num] == null)
					{
						break;
					}
					_elements.Add(elements[num]);
					num++;
					continue;
				}
				return;
			}
			throw new ArgumentNullException("elements", SR.Get(SRID.ParamsEntryNullIllegal));
		}

		/// <summary>Adds an <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsElement" /> to an <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsRule" /> object.</summary>
		/// <param name="element">An object that inherits from <see cref="T:System.Speech.Recognition.SrgsGrammar.SrgsElement" /> and specifies what can be recognized.</param>
		/// <exception cref="T:System.ArgumentNullException">
		///   <paramref name="element" /> is <see langword="null" />.</exception>
		public void Add(SrgsElement element)
		{
			Helpers.ThrowIfNull(element, "element");
			Elements.Add(element);
		}

		internal void WriteSrgs(XmlWriter writer)
		{
			if (Elements.Count == 0)
			{
				XmlParser.ThrowSrgsException(SRID.InvalidEmptyRule, "rule", _id);
			}
			writer.WriteStartElement("rule");
			writer.WriteAttributeString("id", _id);
			if (_isScopeSet)
			{
				switch (_scope)
				{
				case SrgsRuleScope.Private:
					writer.WriteAttributeString("scope", "private");
					break;
				case SrgsRuleScope.Public:
					writer.WriteAttributeString("scope", "public");
					break;
				}
			}
			if (_baseclass != null)
			{
				writer.WriteAttributeString("sapi", "baseclass", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", _baseclass);
			}
			if (_dynamic != RuleDynamic.NotSet)
			{
				writer.WriteAttributeString("sapi", "dynamic", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", (_dynamic == RuleDynamic.True) ? "true" : "false");
			}
			if (OnInit != null)
			{
				writer.WriteAttributeString("sapi", "onInit", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", OnInit);
			}
			if (OnParse != null)
			{
				writer.WriteAttributeString("sapi", "onParse", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", OnParse);
			}
			if (OnError != null)
			{
				writer.WriteAttributeString("sapi", "onError", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", OnError);
			}
			if (OnRecognition != null)
			{
				writer.WriteAttributeString("sapi", "onRecognition", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", OnRecognition);
			}
			Type right = null;
			foreach (SrgsElement element in _elements)
			{
				Type type = element.GetType();
				if (type == typeof(SrgsText) && type == right)
				{
					writer.WriteString(" ");
				}
				right = type;
				element.WriteSrgs(writer);
			}
			writer.WriteEndElement();
			if (HasCode)
			{
				WriteScriptElement(writer, _script);
			}
		}

		internal void Validate(SrgsGrammar grammar)
		{
			bool flag = HasCode || _onInit != null || _onParse != null || _onError != null || _onRecognition != null || _baseclass != null;
			grammar._fContainsCode |= flag;
			grammar.HasSapiExtension |= flag;
			if (_dynamic != RuleDynamic.NotSet)
			{
				grammar.HasSapiExtension = true;
			}
			if (OnInit != null && Scope != 0)
			{
				XmlParser.ThrowSrgsException(SRID.OnInitOnPublicRule, "OnInit", Id);
			}
			if (OnRecognition != null && Scope != 0)
			{
				XmlParser.ThrowSrgsException(SRID.OnInitOnPublicRule, "OnRecognition", Id);
			}
			foreach (SrgsElement element in _elements)
			{
				element.Validate(grammar);
			}
		}

		void IElement.PostParse(IElement grammar)
		{
			((SrgsGrammar)grammar).Rules.Add(this);
		}

		void IRule.CreateScript(IGrammar grammar, string rule, string method, RuleMethodScript type)
		{
			switch (type)
			{
			case RuleMethodScript.onInit:
				_onInit = method;
				break;
			case RuleMethodScript.onParse:
				_onParse = method;
				break;
			case RuleMethodScript.onRecognition:
				_onRecognition = method;
				break;
			case RuleMethodScript.onError:
				_onError = method;
				break;
			}
		}

		private void WriteScriptElement(XmlWriter writer, string sCode)
		{
			writer.WriteStartElement("sapi", "script", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions");
			writer.WriteAttributeString("sapi", "rule", "http://schemas.microsoft.com/Speech/2002/06/SRGSExtensions", _id);
			writer.WriteCData(sCode);
			writer.WriteEndElement();
		}

		private void ValidateIdentifier(string s)
		{
			if (s == _id)
			{
				XmlParser.ThrowSrgsException(SRID.ConstructorNotAllowed, _id);
			}
			if (s != null && (s.IndexOfAny(invalidChars) >= 0 || s.Length == 0))
			{
				XmlParser.ThrowSrgsException(SRID.InvalidMethodName);
			}
		}
	}
}
