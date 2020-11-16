namespace System.Speech.Internal.SapiInterop
{
	internal static class SapiConstants
	{
		internal const string SPPROP_RESPONSE_SPEED = "ResponseSpeed";

		internal const string SPPROP_COMPLEX_RESPONSE_SPEED = "ComplexResponseSpeed";

		internal const string SPPROP_CFG_CONFIDENCE_REJECTION_THRESHOLD = "CFGConfidenceRejectionThreshold";

		internal const uint SPDF_ALL = 255u;

		internal static SRID SapiErrorCode2SRID(SAPIErrorCodes code)
		{
			if (code >= SAPIErrorCodes.SPERR_FIRST && code <= SAPIErrorCodes.SPERR_LAST)
			{
				return (SRID)(258 + (code - -2147201023));
			}
			switch (code)
			{
			case SAPIErrorCodes.SP_NO_RULE_ACTIVE:
				return SRID.SapiErrorNoRuleActive;
			case SAPIErrorCodes.SP_NO_RULES_TO_ACTIVATE:
				return SRID.SapiErrorNoRulesToActivate;
			case SAPIErrorCodes.SP_NO_PARSE_FOUND:
				return SRID.NoParseFound;
			case SAPIErrorCodes.S_FALSE:
				return SRID.UnexpectedError;
			default:
				return (SRID)(-1);
			}
		}
	}
}
