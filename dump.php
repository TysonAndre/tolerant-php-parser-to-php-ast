<?php

if (file_exists(__DIR__ . "/../../../vendor/autoload.php")) {
    require __DIR__ . "/../../../vendor/autoload.php";
} else {
    require __DIR__ . "/vendor/autoload.php";
}
use Microsoft\PhpParser\{DiagnosticsProvider, Node, Parser, PositionUtilities};

function main() {
    error_reporting(E_ALL);
    global $argv;

    if (count($argv) !== 2) {
        fprintf(STDERR, "Usage: %s 'expression'\n", $argv[0]);
        exit(1);
    }
    // Instantiate new parser instance
    $parser = new Parser();
    $expr = $argv[1];
    if ($expr[0] ?? '' !== '<') {
        $expr = '<' . '?php ' . $expr;
    }

    // Return and print an AST from string contents
    $astNode = $parser->parseSourceFile($expr);
    foreach ($astNode->getDescendantNodes() as $descendant) {
        // echo "unsetting " . get_class($descendant) . $descendant->getStart() . "\n";
        unset($descendant->parent);
    }

    $astNode->parent = null;
    var_dump($astNode->statementList);
}
main();
