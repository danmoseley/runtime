namespace System.Speech.Synthesis.TtsEngine
{
	[Flags]
	internal enum SPEAKFLAGS
	{
		SPF_DEFAULT = 0x0,
		SPF_ASYNC = 0x1,
		SPF_PURGEBEFORESPEAK = 0x2,
		SPF_IS_FILENAME = 0x4,
		SPF_IS_XML = 0x8,
		SPF_IS_NOT_XML = 0x10,
		SPF_PERSIST_XML = 0x20,
		SPF_NLP_SPEAK_PUNC = 0x40,
		SPF_PARSE_SAPI = 0x80,
		SPF_PARSE_SSML = 0x100
	}
}
