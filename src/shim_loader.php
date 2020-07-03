<?php

declare(strict_types=1);

// If php-ast isn't loaded already, then load this file to generate equivalent
// class, constant, and function definitions.
// TODO: Node->getText() provide optional file contents, Token->getText provide mandatory file contents
if (!class_exists('\ast\Node')) {
    require_once __DIR__ . '/ast_shim.php';
}
// Define node kinds that may be absent
if (!\defined('ast\AST_PROP_GROUP')) {
    \define('ast\AST_PROP_GROUP', 545);
}
if (!\defined('ast\AST_CLASS_NAME')) {
    \define('ast\AST_CLASS_NAME', 287);
}
if (!\defined('ast\AST_ARROW_FUNC')) {
    \define('ast\AST_ARROW_FUNC', 71);
}
if (!\defined('ast\AST_TYPE_UNION')) {
    \define('ast\AST_TYPE_UNION', 254);
}
if (!\defined('ast\AST_ATTRIBUTE_LIST')) {
    // @phan-suppress-next-line PhanUnreferencedConstant TODO support attributes
    \define('ast\AST_ATTRIBUTE_LIST', 253);
}
if (!\defined('ast\AST_ATTRIBUTE')) {
    // @phan-suppress-next-line PhanUnreferencedConstant TODO support attributes
    \define('ast\AST_ATTRIBUTE', 0x2fb);
}
// Define flags
if (!\defined('ast\flags\DIM_ALTERNATIVE_SYNTAX')) {
    \define('ast\flags\DIM_ALTERNATIVE_SYNTAX', 1 << 1);
}
if (!\defined('ast\flags\PARENTHESIZED_CONDITIONAL')) {
    \define('ast\flags\PARENTHESIZED_CONDITIONAL', 1);
}
if (!\defined('ast\flags\TYPE_FALSE')) {
    \define('ast\flags\TYPE_FALSE', 2);
}
if (!\defined('ast\flags\TYPE_STATIC')) {
    \define('ast\flags\TYPE_STATIC', \PHP_MAJOR_VERSION >= 80000 ? 15 : 20);
}
