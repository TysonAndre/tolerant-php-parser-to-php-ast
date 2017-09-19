<?php declare(strict_types=1);
/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Tyson Andre
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

if (file_exists(__DIR__ . "/../../../vendor/autoload.php")) {
    require __DIR__ . "/../../../vendor/autoload.php";
} else {
    require __DIR__ . "/vendor/autoload.php";
}
use Microsoft\PhpParser\{DiagnosticsProvider, Node, Token, Parser, PositionUtilities};

dump_main();

// Dumps a snippet provided on stdin.
function dump_main() {
    error_reporting(E_ALL);
    global $argv;

    if (count($argv) !== 2) {
        $help = <<<"EOB"
Usage: {$argv[0]} 'snippet'
E.g.
  {$argv[0]} '2+2;'
  {$argv[0]} '<?php function test() {}'

EOB;
        echo $help;
        exit(1);
    }
    $expr = $argv[1];
    // Guess if this is a snippet or file contents
    if (($expr[0] ?? '') !== '<') {
        $expr = '<' . '?php ' . $expr;
    }

    dump_expr($expr);
}

function dump_expr(string $expr) {
    // Instantiate new parser instance
    $parser = new Parser();
    // Return and print an AST from string contents
    $ast_node = $parser->parseSourceFile($expr);
    foreach ($ast_node->getDescendantNodes() as $descendant) {
        // echo "unsetting " . get_class($descendant) . $descendant->getStart() . "\n";
        unset($descendant->parent);
    }

    $ast_node->parent = null;
    unset($ast_node->statementList[0]);
    $dumper = new ASTDumper($expr, false, true, '    ');
    $dumper->dumpTree($ast_node);
    // var_export($ast_node->statementList);
    echo "\n";
}

class ASTDumper {
    /** @var string */
    private $file_contents;
    /** @var bool */
    private $include_offset;
    /** @var bool */
    private $include_token_kind;
    /** @var string */
    private $indent;

    public function __construct(string $file_contents, bool $include_offset = false, bool $include_token_kind = false, string $indent = '    ') {
        $this->file_contents = $file_contents;
        $this->include_offset = $include_offset;
        $this->include_token_kind = $include_token_kind;
        $this->indent = $indent;
    }

    public function dumpClassName(Node $ast_node) : string {
        $name = get_class($ast_node);
        if (stripos($name, 'Microsoft\\PhpParser\\') === 0) {
            $name = substr($name, 20);
        }
        return $name;
    }

    /**
     * @param Node|Token $ast_node
     * @param string $padding (to be echoed before the current node
     * @return void
     */
    public function dumpTree($ast_node, string $key = '', string $padding = '') {
        if ($ast_node instanceof Node) {
            $offset = $ast_node->getStart();
            echo $padding . ($key !== '' ? $key . ': ' : '') . $this->dumpClassName($ast_node) . ($this->include_offset ? " (@" . $offset . ")" : "") . "\n";
            foreach ($ast_node->getChildNodesAndTokens() as $name => $child) {
                $this->dumpTree($child, $name, $padding . $this->indent);
            }
        } else if ($ast_node instanceof Token) {
            echo $padding . ($key !== '' ? $key . ': ' : '') . "Token: " . $ast_node->getTokenKindNameFromValue($ast_node->kind) . ($this->include_token_kind ? '(' . $ast_node->kind . ')' : '') . ': ' . json_encode(substr($this->file_contents, $ast_node->fullStart, $ast_node->length)) . "\n";
        } else {
            echo "Unexpected type of $ast_node was seen\n";
            var_export($ast_node);
            exit(2);
        }
    }
}
