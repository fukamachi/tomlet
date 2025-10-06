# Common Lisp TOML Libraries Comparison

Comparison of TOML parser libraries for Common Lisp against the official TOML v1.0.0 test suite.

**Test Date**: 2025-10-06
**Test Suite**: [toml-lang/toml-test](https://github.com/toml-lang/toml-test) v1.6.0 (TOML v1.0.0)
**Lisp Implementation**: SBCL 2.4.9

## Test Results

| Library | TOML Version | Commit | Tests Passed | Tests Failed | Pass Rate |
|---------|--------------|--------|--------------|--------------|-----------|
| tomlet | v1.0.0 | 7886cf6 | 734/734 | 0 | 100% |
| clop | v1.0.0 | c0c3fe7 | 730/734 | 4 | 99.5% |
| parcom/toml | v1.0.0 | e29a293 | 594/733 | 139 | 81.0% |
| cl-toml | v0.4.0 | - | - | - | Not tested |

**Note**:
- tomlet, clop, and parcom/toml all tested against the same test suite (commit 1d35870, 734 tests total)
- cl-toml supports TOML v0.4.0 (older specification, not compatible with v1.0.0 test suite)
- tomlet: 734 tests (205 valid + 529 invalid)
- clop: 734 tests (205 valid + 529 invalid), 4 failures on high Unicode codepoints >U+FFFF
- parcom: 733 tests (204 valid + 529 invalid), 1 test skipped (valid/float/inf-and-nan.toml)

## Platform Support

### tomlet
- SBCL: All 734 tests pass
- CCL: All 734 tests pass
- ABCL: 723/734 pass (11 skipped due to platform limitation with Unicode codepoints >U+FFFF)
- ECL: 733/734 pass (1 skipped due to platform limitation with Unicode codepoints >U+FFFF)

### clop
- SBCL: 730/734 pass (4 failures on high Unicode codepoints >U+FFFF)

### parcom/toml
- SBCL: 594/733 pass (139 failures, 1 test skipped)

### cl-toml
- Not tested in this comparison

## Features Not Available

### tomlet
- None identified (100% TOML v1.0.0 compliance)

### clop
- Unicode codepoints beyond U+FFFF - 4 tests
  - valid/comment/nonascii.toml
  - valid/key/quoted-unicode.toml
  - valid/multibyte.toml
  - valid/string/quoted-unicode.toml

### parcom/toml
139 test failures (97 invalid tests accepted + 42 valid tests failed):

**Invalid tests incorrectly accepted (97 tests):**
- Control characters under invalid/control/ (21 tests)
- String escapes under invalid/string/ (52 tests)
- Table/key errors under invalid/key/, invalid/table/, invalid/inline-table/ (24 tests)

**Valid tests failed (42 tests):**
- Multiline strings under valid/string/, valid/spec-1.0.0/ (13 tests)
- Table semantics under valid/table/, valid/spec-1.0.0/ (11 tests)
- Integer literals under valid/integer/ (2 tests)
- Special floats: valid/spec-1.0.0/float-2.toml (1 test)
- Comments under valid/comment/ (3 tests)
- Arrays under valid/array/ (3 tests)
- Others under valid/datetime/, valid/float/, valid/inline-table/, valid/key/ (9 tests)

### cl-toml
- TOML v1.0.0 features (supports v0.4.0 only)
- Specific v1.0.0 features unavailable:
  - Dotted keys
  - Inline table newlines
  - Some datetime formats added in v1.0.0

## Library Information

### tomlet
- **Repository**: https://github.com/fukamachi/tomlet
- **License**: MIT
- **Dependencies**: cl-ppcre
- **Portability**: Implementation-specific code only in float-utils.lisp for special float values (inf, nan) with reader conditionals for SBCL, CCL, ECL, ABCL, Allegro, LispWorks, and Clasp

### clop
- **Repository**: https://github.com/sheepduke/clop
- **License**: MIT
- **Dependencies**: alexandria, esrap, parse-number, local-time, str
- **Portability**: Fully portable, no implementation-specific code

### parcom/toml
- **Repository**: https://github.com/fosskers/parcom
- **License**: Apache-2.0
- **Part of**: parcom parser combinator library
- **Portability**: Fully portable, no implementation-specific code

### cl-toml
- **Repository**: https://github.com/cxxxr/cl-toml
- **License**: MIT
- **TOML Version**: v0.4.0
- **Portability**: Fully portable, no implementation-specific code

## References

- TOML Specification: https://toml.io/en/v1.0.0
- TOML Test Suite: https://github.com/toml-lang/toml-test
