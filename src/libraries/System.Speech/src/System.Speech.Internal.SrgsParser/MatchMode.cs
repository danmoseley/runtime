namespace System.Speech.Internal.SrgsParser
{
	internal enum MatchMode
	{
		AllWords = 0,
		Subsequence = 1,
		OrderedSubset = 3,
		SubsequenceContentRequired = 5,
		OrderedSubsetContentRequired = 7
	}
}
