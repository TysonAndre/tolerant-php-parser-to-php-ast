<?php

declare(strict_types=1);

namespace TolerantASTConverter\Tests;

use AssertionError;
use ast;
use RecursiveDirectoryIterator;
use RecursiveIteratorIterator;
use RuntimeException;
use TolerantASTConverter\NodeDumper;
use TolerantASTConverter\TolerantASTConverter;

use function count;
use function get_class;
use function in_array;
use function is_array;
use function is_int;
use function is_string;

require_once __DIR__ . '/../../src/util.php';
require_once __DIR__ . '/../../src/shim_loader.php';

/**
 * Tests that the polyfill works with valid ASTs
 */
class ConversionTest extends \PHPUnit\Framework\TestCase
{
    /**
     * @return list<string>
     * @suppress PhanPluginUnknownObjectMethodCall
     */
    protected function scanSourceDirForPHP(string $source_dir): array
    {
        $files = [];
        foreach (new RecursiveIteratorIterator(new RecursiveDirectoryIterator($source_dir)) as $file_path => $file_info) {
            $filename = $file_info->getFilename();
            if ($filename &&
                !in_array($filename, ['.', '..'], true) &&
                \substr($filename, 0, 1) !== '.' &&
                \strpos($filename, '.') !== false &&
                (\pathinfo($filename)['extension'] ?? '') === 'php') {
                $files[] = $file_path;
            }
        }
        if (count($files) === 0) {
            throw new \InvalidArgumentException(\sprintf("RecursiveDirectoryIterator iteration returned no files for %s\n", $source_dir));
        }
        return $files;
    }

    /**
     * @return bool does php-ast support $ast_version
     */
    public static function hasNativeASTSupport(int $ast_version): bool
    {
        try {
            ast\parse_code('', $ast_version);
            return true;
        } catch (\LogicException $_) {
            return false;
        }
    }

    /**
     * This is used to sort by token count, so that the failures with the fewest token
     * (i.e. simplest ASTs) appear first.
     * @param string[] $files
     */
    private static function sortByTokenCount(array &$files): void
    {
        $token_counts = [];
        foreach ($files as $file) {
            $contents = \file_get_contents($file);
            if (!is_string($contents)) {
                throw new AssertionError("Failed to read $file");
            }
            $token_counts[$file] = count(\token_get_all($contents));
        }
        \usort($files, static function (string $path1, string $path2) use ($token_counts): int {
            return $token_counts[$path1] <=> $token_counts[$path2];
        });
    }

    /**
     * Asserts that valid files get parsed the same way by php-ast and the polyfill.
     *
     * @return array{0:string,1:int}[] array of [string $file_path, int $ast_version]
     */
    public function astValidFileExampleProvider(): array
    {
        $tests = [];
        // @phan-suppress-next-line PhanPossiblyFalseTypeArgumentInternal
        $source_dir = \dirname(\realpath(__DIR__), 2) . '/test_files/src';
        $paths = $this->scanSourceDirForPHP($source_dir);

        self::sortByTokenCount($paths);
        $supports70 = self::hasNativeASTSupport(70);
        if (!$supports70) {
            throw new RuntimeException("Version 70 is not natively supported");
        }
        foreach ($paths as $path) {
            $tests[] = [$path, 70];
        }
        return $tests;
    }

    /**
     * @param ast\Node|int|string|float|null $node
     */
    private static function normalizeOriginalAST($node): void
    {
        if ($node instanceof ast\Node) {
            $kind = $node->kind;
            if ($kind === ast\AST_FUNC_DECL || $kind === ast\AST_METHOD) {
                // https://github.com/nikic/php-ast/issues/64
                $node->flags &= ~(0x800000);
            }
            foreach ($node->children as $c) {
                self::normalizeOriginalAST($c);
            }
            return;
        } elseif (\is_array($node)) {
            foreach ($node as $c) {
                self::normalizeOriginalAST($c);
            }
        }
    }

    // TODO: TolerantPHPParser gets more information than PHP-Parser for statement lists,
    // so this step may be unnecessary
    /**
     * Set all of the line numbers to constants,
     * so that minor differences in line numbers won't cause tests to fail.
     */
    public static function normalizeLineNumbers(ast\Node $node): ast\Node
    {
        $node = clone($node);
        if (is_array($node->children)) {
            foreach ($node->children as $k => $v) {
                if ($v instanceof ast\Node) {
                    $node->children[$k] = self::normalizeLineNumbers($v);
                }
            }
        }
        $node->lineno = 1;
        return $node;
    }

    private const FUNCTION_DECLARATION_KINDS = [
        ast\AST_FUNC_DECL,
        ast\AST_METHOD,
        ast\AST_CLOSURE,
        ast\AST_ARROW_FUNC,
    ];

    /**
     * Normalizes the flags on function declaration caused by \ast\flags\FUNC_GENERATOR.
     *
     * Phan does not use these flags because they are not natively provided in all PHP versions.
     * TODO: Shouldn't they be available in PHP 7.1+
     * @suppress PhanUndeclaredProperty
     */
    public static function normalizeNodeFlags(ast\Node $node): void
    {
        if (\in_array($node->kind, self::FUNCTION_DECLARATION_KINDS, true)) {
            // Alternately, could make Phan do this.
            $node->flags &= ~ast\flags\FUNC_GENERATOR;
        }
        unset($node->is_not_parenthesized);
        unset($node->polyfill_has_trailing_comma);

        foreach ($node->children as $v) {
            if ($v instanceof ast\Node) {
                self::normalizeNodeFlags($v);
            }
        }
    }

    /** @dataProvider astValidFileExampleProvider */
    public function testFallbackFromParser(string $file_name, int $ast_version): void
    {
        $test_folder_name = \basename(\dirname($file_name));
        if (\PHP_VERSION_ID < 70300 && $test_folder_name === 'php73_or_newer') {
            $this->markTestIncomplete('php-ast cannot parse php7.3 syntax when running in php7.2 or older');
        }
        if (\PHP_VERSION_ID < 70400 && $test_folder_name === 'php74_or_newer') {
            $this->markTestIncomplete('php-ast cannot parse php7.4 syntax when running in php7.3 or older');
        }
        if (\PHP_VERSION_ID < 80000 && $test_folder_name === 'php80_or_newer') {
            $this->markTestIncomplete('php-ast cannot parse php8.0 syntax when running in php7.4 or older');
        }
        if (\PHP_VERSION_ID >= 80000 && \basename($file_name) === 'use_simple.php') {
            $this->markTestIncomplete('php-ast cannot parse php8.0 syntax when running in php7.4 or older');
        }
        $contents = \file_get_contents($file_name);
        if ($contents === false) {
            $this->fail("Failed to read $file_name");
            return;  // unreachable
        }
        try {
            $ast = ast\parse_code($contents, $ast_version, $file_name);
        } catch (\ParseError $e) {
            $this->fail("Failed for $file_name:{$e->getLine()}: {$e->getMessage()}");
            return;  // unreachable
        }
        self::normalizeOriginalAST($ast);
        $this->assertInstanceOf('\ast\Node', $ast, 'Examples must be syntactically valid PHP parsable by php-ast');
        $converter = new TolerantASTConverter();
        $converter->setPHPVersionId(\PHP_VERSION_ID);
        try {
            $fallback_ast = $converter->parseCodeAsPHPAST($contents, $ast_version);
        } catch (\Throwable $e) {
            $code = $e->getCode();
            throw new \RuntimeException("Error parsing $file_name with ast version $ast_version", is_int($code) ? $code : 1, $e);
        }
        $this->assertInstanceOf('\ast\Node', $fallback_ast, 'The fallback must also return a tree of php-ast nodes');

        if ($test_folder_name === 'phan_test_files' || $test_folder_name === 'php-src_tests') {
            $fallback_ast = self::normalizeLineNumbers($fallback_ast);
            $ast          = self::normalizeLineNumbers($ast);
        }
        self::normalizeNodeFlags($ast);
        self::normalizeNodeFlags($fallback_ast);
        // TODO: Remove $ast->parent recursively
        $fallback_ast_repr = \var_export($fallback_ast, true);
        $original_ast_repr = \var_export($ast, true);

        if ($fallback_ast_repr !== $original_ast_repr) {
            $node_dumper = new NodeDumper($contents);
            $node_dumper->setIncludeTokenKind(true);
            $node_dumper->setIncludeOffset(true);
            $php_parser_node = $converter->phpparserParse($contents);
            try {
                $dump = $node_dumper->dumpTreeAsString($php_parser_node);
            } catch (\Throwable $e) {
                $dump = 'could not dump PhpParser Node: ' . get_class($e) . ': ' . $e->getMessage() . "\n" . $e->getTraceAsString();
            }
            $original_ast_dump = \ast_dump($ast);
            try {
                $fallback_ast_dump = \ast_dump($fallback_ast);
            } catch (\Throwable $e) {
                $fallback_ast_dump = 'could not dump php-ast Node: ' . get_class($e) . ': ' . $e->getMessage() . "\n" . $e->getTraceAsString();
            }
            // $parser_export = var_dump($php_parser_node, true);
            $this->assertSame($original_ast_repr, $fallback_ast_repr, <<<EOT
The fallback must return the same tree of php-ast nodes
File: $file_name
Code:
$contents

Original AST:
$original_ast_dump

Fallback AST:
$fallback_ast_dump
PHP-Parser(simplified):
$dump
EOT

            /*
PHP-Parser(unsimplified):
$parser_export
             */);
        }
    }
}
