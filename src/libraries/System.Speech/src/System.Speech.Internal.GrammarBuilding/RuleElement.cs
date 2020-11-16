using System.Speech.Internal.SrgsParser;

namespace System.Speech.Internal.GrammarBuilding
{
	internal sealed class RuleElement : BuilderElements
	{
		private readonly string _name;

		private string _ruleName;

		private IRule _rule;

		internal override string DebugSummary => _name + "=" + base.DebugSummary;

		internal string Name => _name;

		internal string RuleName => _ruleName;

		internal RuleElement(string name)
		{
			_name = name;
		}

		internal RuleElement(GrammarBuilderBase builder, string name)
			: this(name)
		{
			Add(builder);
		}

		public override bool Equals(object obj)
		{
			RuleElement ruleElement = obj as RuleElement;
			if (ruleElement == null)
			{
				return false;
			}
			if (!base.Equals(obj))
			{
				return false;
			}
			return _name == ruleElement._name;
		}

		public override int GetHashCode()
		{
			return base.GetHashCode();
		}

		internal override GrammarBuilderBase Clone()
		{
			RuleElement ruleElement = new RuleElement(_name);
			ruleElement.CloneItems(this);
			return ruleElement;
		}

		internal override IElement CreateElement(IElementFactory elementFactory, IElement parent, IRule rule, IdentifierCollection ruleIds)
		{
			if (_rule == null)
			{
				IGrammar grammar = elementFactory.Grammar;
				_ruleName = ruleIds.CreateNewIdentifier(Name);
				_rule = grammar.CreateRule(_ruleName, RulePublic.False, RuleDynamic.NotSet, hasSCript: false);
				CreateChildrenElements(elementFactory, _rule, ruleIds);
				_rule.PostParse(grammar);
			}
			return _rule;
		}

		internal override int CalcCount(BuilderElements parent)
		{
			_rule = null;
			return base.CalcCount(parent);
		}
	}
}
