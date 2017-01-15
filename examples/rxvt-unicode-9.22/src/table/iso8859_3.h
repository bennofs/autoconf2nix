//
// AUTOMATICALLLY GENERATED by gentables
//
#ifdef ENCODING_EU

static const uint8_t iso8859_3_f_0[] = {
  0xa0, 0x00, 0x00, 0xa3, 0xa4, 0x00, 0x00, 0xa7, 0xa8, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
  0xb0, 0x00, 0xb2, 0xb3, 0xb4, 0xb5, 0x00, 0xb7, 0xb8, 0x00, 0x00, 0x00, 0x00, 0xbd, 0x00, 0x00,
  0xc0, 0xc1, 0xc2, 0x00, 0xc4, 0x00, 0x00, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0x00, 0xd1, 0xd2, 0xd3, 0xd4, 0x00, 0xd6, 0xd7, 0x00, 0xd9, 0xda, 0xdb, 0xdc, 0x00, 0x00, 0xdf,
  0xe0, 0xe1, 0xe2, 0x00, 0xe4, 0x00, 0x00, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
  0x00, 0xf1, 0xf2, 0xf3, 0xf4, 0x00, 0xf6, 0xf7, 0x00, 0xf9, 0xfa, 0xfb, 0xfc
};
static const uint8_t iso8859_3_f_256[] = {
  0xc6, 0xe6, 0xc5, 0xe5, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0xd8, 0xf8, 0xab, 0xbb, 0xd5, 0xf5, 0x00, 0x00, 0xa6, 0xb6, 0xa1, 0xb1,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa9, 0xb9, 0x00, 0x00, 0xac, 0xbc, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0xde, 0xfe, 0xaa, 0xba, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0xdd, 0xfd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0xaf, 0xbf
};

static uint32_t cs_iso8859_3_from_unicode (unicode_t unicode) {
    if (unicode <= 0x009f) return unicode;
    if (unicode == 0x02d8) return 0x00a2;
    if (unicode == 0x02d9) return 0x00ff;
    if (0x00a0 <= unicode && unicode <= 0x00fc)
      return iso8859_3_f_0[unicode - 0x00a0] == 0 ? NOCHAR : iso8859_3_f_0[unicode - 0x00a0];
    if (0x0108 <= unicode && unicode <= 0x017c)
      return iso8859_3_f_256[unicode - 0x0108] == 0 ? NOCHAR : iso8859_3_f_256[unicode - 0x0108];
    return NOCHAR;
}

#else

#define cs_iso8859_3_from_unicode cs_unknown_from_unicode
#define cs_iso8859_3_to_unicode   cs_unknown_to_unicode

#endif