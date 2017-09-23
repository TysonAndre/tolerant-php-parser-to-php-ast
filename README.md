Tolerant-PHP-Parser to php-ast
==============================

[![Build Status](https://travis-ci.org/TysonAndre/tolerant-php-parser-to-php-ast.svg?branch=master)](https://travis-ci.org/TysonAndre/tolerant-php-parser-to-php-ast)

This project uses Microsoft/tolerant-php-parser to generate a tree with error tolerance, then converts from that tree to ast\Node: https://github.com/Microsoft/tolerant-php-parser/issues/113

This is 90% done. Current test cases pass, but some cases aren't yet tested.

- Still being ported from https://github.com/TysonAndre/php-parser-to-php-ast
- The test suite is not yet comprehensive, may need to handle tokens

[Current Issues](https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/issues/)

- The test suite this is based off of covers common cases for Phan, but edge cases still remain.
  See https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/issues/4

Usage
-----

Using it as a slow substitute for php-ast

- [tests/TolerantASTConverter/ConversionTest.php](https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/blob/master/tests/TolerantASTConverter/ConversionTest.php)

Using it as an error-tolerant substitute for php-ast (e.g. for use in IDEs)

- There are currently two modes: omitting errors and adding placeholders (e.g. `__INCOMPLETE_VARIABLE__`).
- Omitting errors only handles some common cases that come up while editing a file.
- Placeholders may change in the future.
- [tests/ASTConverter/ErrorTolerantConversionTest.php](https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/blob/master/tests/ASTConverter/ErrorTolerantConversionTest.php)

Running unit tests
------------------

To run unit tests, you must install [nikic/php-ast](https://github.com/nikic/php-ast) 0.1.5+ (for the expected results to be created).
You must also run `composer install` if you haven't already done so.

- Then run `vendor/bin/phpunit`

Possible Future Enhancements
----------------------------

- Adding a mode to never accept invalid PHP (for completeness)
