using System.Speech.Internal.SrgsParser;
using System.Text;

namespace System.Speech.Internal.GrammarBuilding
{
	internal sealed class OneOfElement : BuilderElements
	{
		internal override string DebugSummary
		{
			get
			{
				StringBuilder stringBuilder = new StringBuilder();
				foreach (GrammarBuilderBase item in base.Items)
				{
					if (stringBuilder.Length > 0)
					{
						stringBuilder.Append(",");
					}
					stringBuilder.Append(item.DebugSummary);
				}
				return "[" + stringBuilder.ToString() + "]";
			}
		}

		internal OneOfElement()
		{
		}

		internal override GrammarBuilderBase Clone()
		{
			OneOfElement oneOfElement = new OneOfElement();
			oneOfElement.CloneItems(this);
			return oneOfElement;
		}

		internal override IElement CreateElement(IElementFactory elementFactory, IElement parent, IRule rule, IdentifierCollection ruleIds)
		{
			IOneOf oneOf = elementFactory.CreateOneOf(parent, rule);
			foreach (GrammarBuilderBase item2 in base.Items)
			{
				ItemElement itemElement = item2 as ItemElement;
				if (itemElement == null)
				{
					itemElement = new ItemElement(item2);
				}
				IItem item = (IItem)itemElement.CreateElement(elementFactory, oneOf, rule, ruleIds);
				item.PostParse(oneOf);
				elementFactory.AddItem(oneOf, item);
			}
			return oneOf;
		}
	}
}
