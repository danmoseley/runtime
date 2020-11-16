using System.Speech.Internal.SrgsParser;

namespace System.Speech.Internal.SrgsCompiler
{
	internal class OneOf : ParseElementCollection, IOneOf, IElement
	{
		private State _startState;

		private State _endState;

		public OneOf(Rule rule, Backend backend)
			: base(backend, rule)
		{
			_startState = _backend.CreateNewState(rule);
			_endState = _backend.CreateNewState(rule);
			_startArc = _backend.EpsilonTransition(1f);
			_startArc.End = _startState;
			_endArc = _backend.EpsilonTransition(1f);
			_endArc.Start = _endState;
		}

		void IElement.PostParse(IElement parentElement)
		{
			if (_startArc.End.OutArcs.IsEmpty)
			{
				XmlParser.ThrowSrgsException(SRID.EmptyOneOf);
			}
			_startArc = ParseElementCollection.TrimStart(_startArc, _backend);
			_endArc = ParseElementCollection.TrimEnd(_endArc, _backend);
			PostParse((ParseElementCollection)parentElement);
		}

		internal override void AddArc(Arc start, Arc end)
		{
			start = ParseElementCollection.TrimStart(start, _backend);
			end = ParseElementCollection.TrimEnd(end, _backend);
			State start2 = end.Start;
			State end2 = start.End;
			if ((start.IsEpsilonTransition & start.IsPropertylessTransition) && end2 != null && end2.InArcs.IsEmpty)
			{
				start.End = null;
				_backend.MoveOutputTransitionsAndDeleteState(end2, _startState);
			}
			else
			{
				start.Start = _startState;
			}
			if ((end.IsEpsilonTransition & end.IsPropertylessTransition) && start2 != null && start2.OutArcs.IsEmpty)
			{
				end.Start = null;
				_backend.MoveInputTransitionsAndDeleteState(start2, _endState);
			}
			else
			{
				end.End = _endState;
			}
		}
	}
}
