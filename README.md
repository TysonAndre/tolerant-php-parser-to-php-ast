Tolerant-PHP-Parser to php-ast
==============================

[![Build Status](https://travis-ci.org/TysonAndre/tolerant-php-parser-to-php-ast.svg?branch=master)](https://travis-ci.org/TysonAndre/tolerant-php-parser-to-php-ast)
[![Latest Stable Version](https://img.shields.io/packagist/v/tysonandre/tolerant-php-parser-to-php-ast.svg)](https://packagist.org/packages/tysonandre/tolerant-php-parser-to-php-ast)
[![License](https://img.shields.io/packagist/l/tysonandre/tolerant-php-parser-to-php-ast.svg)](https://github.com/tysonandre/tolerant-php-parser-to-php-astn/blob/master/LICENSE)

This project uses Microsoft/tolerant-php-parser to generate a tree with error tolerance, then converts from that tree to ast\Node from [php-ast](https://github.com/nikic/php-ast)

This release (0.1.0) supports AST version 70.

Usage
-----

Using it as a slow substitute for php-ast

- [tests/TolerantASTConverter/ConversionTest.php](https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/blob/master/tests/TolerantASTConverter/ConversionTest.php)

Using it as an error-tolerant substitute for php-ast (e.g. for use in IDEs)

- There are currently two modes: omitting errors and adding placeholders (e.g. `__INCOMPLETE_VARIABLE__`).
- Omitting errors only handles some common cases that come up while editing a file.
- Placeholders may change in the future.
- [tests/ASTConverter/ErrorTolerantConversionTest.php](https://github.com/TysonAndre/tolerant-php-parser-to-php-ast/blob/master/tests/ASTConverter/ErrorTolerantConversionTest.php)
- [Phan's](https://github.com/phan/phan) Language Server uses this code to do that.

Running unit tests
------------------

To run unit tests, you must install [nikic/php-ast](https://github.com/nikic/php-ast) 1.0.1+ (for the expected results to be created).
You must also run `composer install` if you haven't already done so.

- Then run `vendor/bin/phpunit`

Possible Future Enhancements
----------------------------

- Adding a mode to never accept invalid PHP (for completeness)
