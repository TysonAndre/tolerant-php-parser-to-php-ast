<?php declare(strict_types = 1);
namespace TolerantASTConverter\Tests;

use TolerantASTConverter\TolerantASTConverter;
use TolerantASTConverter\NodeDumper;

require_once __DIR__ . '/../../src/util.php';

class ErrorTolerantConversionTest extends \PHPUnit\Framework\TestCase
{
    public function testIncompleteVar()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $a = $
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {

}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents);
    }

    public function testIncompleteVarWithPlaceholderShort()
    {
        $incomplete_contents = <<<'EOT'
<?php
$a = $
EOT;
        $valid_contents = <<<'EOT'
<?php
$a = $__INCOMPLETE_VARIABLE__;
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testIncompleteVarWithPlaceholder()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $a = $
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $a = $__INCOMPLETE_VARIABLE__;
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testIncompleteProperty()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $c;
  $a = $b->
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $c;

}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents);
    }

    public function testIncompletePropertyWithPlaceholder()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $c;
  $a = $b->
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $c;
  $a = $b->__INCOMPLETE_PROPERTY__;
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testIncompleteMethod()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $b;
  $a = Bar::
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b;

}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents);
    }

    public function testIncompleteMethodWithPlaceholder()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $b;
  $a = Bar::
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b;
  $a = Bar::__INCOMPLETE_CLASS_CONST__;
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testMiscNoise()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $b;
  |
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b;

}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents);
    }

    public function testMiscNoiseWithPlaceholders()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  $b;
  |
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b;

}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testIncompleteArithmeticWithPlaceholders()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  ($b * $c) +
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b * $c + \__INCOMPLETE_EXPR__;
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, true);
    }

    public function testIncompleteArithmeticWithoutPlaceholders()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
  ($b * $c) +
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
  $b * $c;
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents, false);
    }

    public function testMissingSemicolon()
    {
        $incomplete_contents = <<<'EOT'
<?php
function foo() {
    $y = 3
    $x = intdiv(3, 2);
}
EOT;
        $valid_contents = <<<'EOT'
<?php
function foo() {
    $y = 3;
    $x = intdiv(3, 2);
}
EOT;
        $this->_testFallbackFromParser($incomplete_contents, $valid_contents);
    }

// Another test (Won't work with php-parser, might work with tolerant-php-parser
/**
        $incomplete_contents = <<<'EOT'
<?php
class C{
    public function foo() {
        $x = 3;


    public function bar() {
    }
}
EOT;
        $valid_contents = <<<'EOT'
<?php
class C{
    public function foo() {
        $x = 3;
    }

    public function bar() {
    }
}
EOT;
 */

    private function _testFallbackFromParser(string $incomplete_contents, string $valid_contents, bool $should_add_placeholders = false)
    {
        $supports40 = ConversionTest::hasNativeASTSupport(40);
        $supports50 = ConversionTest::hasNativeASTSupport(50);
        if (!($supports40 || $supports50)) {
            $this->fail('No supported AST versions to test');
        }
        if ($supports40) {
            $this->_testFallbackFromParserForASTVersion($incomplete_contents, $valid_contents, 40, $should_add_placeholders);
        }
        if ($supports50) {
            $this->_testFallbackFromParserForASTVersion($incomplete_contents, $valid_contents, 50, $should_add_placeholders);
        }
    }

    private function _testFallbackFromParserForASTVersion(string $incomplete_contents, string $valid_contents, int $ast_version, bool $should_add_placeholders)
    {
        $ast = \ast\parse_code($valid_contents, $ast_version);
        $this->assertInstanceOf('\ast\Node', $ast, 'Examples(for validContents) must be syntactically valid PHP parseable by php-ast');
        $errors = [];
        $converter = new TolerantASTConverter();
        $converter->setShouldAddPlaceholders($should_add_placeholders);
        $php_parser_node = $converter->phpParserParse($incomplete_contents, $errors);
        $fallback_ast = $converter->phpParserToPhpAst($php_parser_node, $ast_version, $incomplete_contents);
        $this->assertInstanceOf('\ast\Node', $fallback_ast, 'The fallback must also return a tree of php-ast nodes');
        $fallback_ast_repr = var_export($fallback_ast, true);
        $original_ast_repr = var_export($ast, true);

        if ($fallback_ast_repr !== $original_ast_repr) {
            $placeholders_used_str = $should_add_placeholders ? 'Yes' : 'No';
            $dumper = new NodeDumper($incomplete_contents);
            $dumper->setIncludeTokenKind(true);
            $dumper->setIncludeOffset(true);
            $dump = $dumper->dumpTreeAsString($php_parser_node);
            $original_ast_dump = \ast_dump($ast);
            // $parser_export = var_export($php_parser_node, true);
            $this->assertSame($original_ast_repr, $fallback_ast_repr, <<<EOT
The fallback must return the same tree of php-ast nodes
Placeholders Used: $placeholders_used_str
Code:
$incomplete_contents

Closest Valid Code:
$valid_contents

Original AST:
$original_ast_dump

Tolerant-PHP-Parser(simplified):
$dump
EOT

            /*
Tolerant-PHP-Parser(unsimplified):
$parser_export
             */);
        }
    }
}
