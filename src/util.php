<?php declare(strict_types=1);

use ast\flags;
use ast\Node;

const AST_DUMP_LINENOS = 1;

/**
 * Source: https://github.com/nikic/php-ast/blob/master/util.php
 *
 * Returns the information necessary to map the node id to the flag id to the name.
 *
 * @return array{0:associative-array<int,array<int,string>>,1:associative-array<int,array<int,string>>}
 * Returns [string[][] $exclusive, string[][] $combinable].
 */
function get_flag_info() : array
{
    // TODO: Use AST's built in flag info if available.
    static $exclusive, $combinable;
    // Write this in a way that lets Phan infer the value of $combinable at the end.
    if ($exclusive === null) {
        $function_modifiers = [
            flags\MODIFIER_PUBLIC => 'MODIFIER_PUBLIC',
            flags\MODIFIER_PROTECTED => 'MODIFIER_PROTECTED',
            flags\MODIFIER_PRIVATE => 'MODIFIER_PRIVATE',
            flags\MODIFIER_STATIC => 'MODIFIER_STATIC',
            flags\MODIFIER_ABSTRACT => 'MODIFIER_ABSTRACT',
            flags\MODIFIER_FINAL => 'MODIFIER_FINAL',
            flags\FUNC_RETURNS_REF => 'FUNC_RETURNS_REF',
            flags\FUNC_GENERATOR => 'FUNC_GENERATOR',
        ];
        $property_modifiers = [
            flags\MODIFIER_PUBLIC => 'MODIFIER_PUBLIC',
            flags\MODIFIER_PROTECTED => 'MODIFIER_PROTECTED',
            flags\MODIFIER_PRIVATE => 'MODIFIER_PRIVATE',
            flags\MODIFIER_STATIC => 'MODIFIER_STATIC',
            flags\MODIFIER_ABSTRACT => 'MODIFIER_ABSTRACT',
            flags\MODIFIER_FINAL => 'MODIFIER_FINAL',
        ];
        $types = [
            flags\TYPE_NULL => 'TYPE_NULL',
            flags\TYPE_BOOL => 'TYPE_BOOL',
            flags\TYPE_LONG => 'TYPE_LONG',
            flags\TYPE_DOUBLE => 'TYPE_DOUBLE',
            flags\TYPE_STRING => 'TYPE_STRING',
            flags\TYPE_ARRAY => 'TYPE_ARRAY',
            flags\TYPE_OBJECT => 'TYPE_OBJECT',
            flags\TYPE_CALLABLE => 'TYPE_CALLABLE',
            flags\TYPE_VOID => 'TYPE_VOID',
            flags\TYPE_ITERABLE => 'TYPE_ITERABLE',
        ];
        $use_types = [
            flags\USE_NORMAL => 'USE_NORMAL',
            flags\USE_FUNCTION => 'USE_FUNCTION',
            flags\USE_CONST => 'USE_CONST',
        ];
        $shared_binary_ops = [
            flags\BINARY_BITWISE_OR => 'BINARY_BITWISE_OR',
            flags\BINARY_BITWISE_AND => 'BINARY_BITWISE_AND',
            flags\BINARY_BITWISE_XOR => 'BINARY_BITWISE_XOR',
            flags\BINARY_CONCAT => 'BINARY_CONCAT',
            flags\BINARY_ADD => 'BINARY_ADD',
            flags\BINARY_SUB => 'BINARY_SUB',
            flags\BINARY_MUL => 'BINARY_MUL',
            flags\BINARY_DIV => 'BINARY_DIV',
            flags\BINARY_MOD => 'BINARY_MOD',
            flags\BINARY_POW => 'BINARY_POW',
            flags\BINARY_SHIFT_LEFT => 'BINARY_SHIFT_LEFT',
            flags\BINARY_SHIFT_RIGHT => 'BINARY_SHIFT_RIGHT',
            flags\BINARY_COALESCE => 'BINARY_COALESCE',
        ];

        $exclusive = [
            ast\AST_NAME => [
                flags\NAME_FQ => 'NAME_FQ',
                flags\NAME_NOT_FQ => 'NAME_NOT_FQ',
                flags\NAME_RELATIVE => 'NAME_RELATIVE',
            ],
            ast\AST_CLASS => [
                flags\CLASS_ABSTRACT => 'CLASS_ABSTRACT',
                flags\CLASS_FINAL => 'CLASS_FINAL',
                flags\CLASS_TRAIT => 'CLASS_TRAIT',
                flags\CLASS_INTERFACE => 'CLASS_INTERFACE',
                flags\CLASS_ANONYMOUS => 'CLASS_ANONYMOUS',
            ],
            ast\AST_TYPE => $types,
            ast\AST_CAST => $types,
            ast\AST_UNARY_OP => [
                flags\UNARY_BOOL_NOT => 'UNARY_BOOL_NOT',
                flags\UNARY_BITWISE_NOT => 'UNARY_BITWISE_NOT',
                flags\UNARY_MINUS => 'UNARY_MINUS',
                flags\UNARY_PLUS => 'UNARY_PLUS',
                flags\UNARY_SILENCE => 'UNARY_SILENCE',
            ],
            ast\AST_BINARY_OP => $shared_binary_ops + [
                flags\BINARY_BOOL_AND => 'BINARY_BOOL_AND',
                flags\BINARY_BOOL_OR => 'BINARY_BOOL_OR',
                flags\BINARY_BOOL_XOR => 'BINARY_BOOL_XOR',
                flags\BINARY_IS_IDENTICAL => 'BINARY_IS_IDENTICAL',
                flags\BINARY_IS_NOT_IDENTICAL => 'BINARY_IS_NOT_IDENTICAL',
                flags\BINARY_IS_EQUAL => 'BINARY_IS_EQUAL',
                flags\BINARY_IS_NOT_EQUAL => 'BINARY_IS_NOT_EQUAL',
                flags\BINARY_IS_SMALLER => 'BINARY_IS_SMALLER',
                flags\BINARY_IS_SMALLER_OR_EQUAL => 'BINARY_IS_SMALLER_OR_EQUAL',
                flags\BINARY_IS_GREATER => 'BINARY_IS_GREATER',
                flags\BINARY_IS_GREATER_OR_EQUAL => 'BINARY_IS_GREATER_OR_EQUAL',
                flags\BINARY_SPACESHIP => 'BINARY_SPACESHIP',
            ],
            ast\AST_ASSIGN_OP => $shared_binary_ops,
            ast\AST_MAGIC_CONST => [
                flags\MAGIC_LINE => 'MAGIC_LINE',
                flags\MAGIC_FILE => 'MAGIC_FILE',
                flags\MAGIC_DIR => 'MAGIC_DIR',
                flags\MAGIC_NAMESPACE => 'MAGIC_NAMESPACE',
                flags\MAGIC_FUNCTION => 'MAGIC_FUNCTION',
                flags\MAGIC_METHOD => 'MAGIC_METHOD',
                flags\MAGIC_CLASS => 'MAGIC_CLASS',
                flags\MAGIC_TRAIT => 'MAGIC_TRAIT',
            ],
            ast\AST_USE => $use_types,
            ast\AST_GROUP_USE => $use_types,
            ast\AST_USE_ELEM => $use_types,
            ast\AST_INCLUDE_OR_EVAL => [
                flags\EXEC_EVAL => 'EXEC_EVAL',
                flags\EXEC_INCLUDE => 'EXEC_INCLUDE',
                flags\EXEC_INCLUDE_ONCE => 'EXEC_INCLUDE_ONCE',
                flags\EXEC_REQUIRE => 'EXEC_REQUIRE',
                flags\EXEC_REQUIRE_ONCE => 'EXEC_REQUIRE_ONCE',
            ],
            ast\AST_ARRAY => [
                flags\ARRAY_SYNTAX_LIST => 'ARRAY_SYNTAX_LIST',
                flags\ARRAY_SYNTAX_LONG => 'ARRAY_SYNTAX_LONG',
                flags\ARRAY_SYNTAX_SHORT => 'ARRAY_SYNTAX_SHORT',
            ],
            ast\AST_ARRAY_ELEM => [
                flags\ARRAY_ELEM_REF => 'ARRAY_ELEM_REF',
            ],
            ast\AST_CLOSURE_VAR => [
                flags\CLOSURE_USE_REF => 'CLOSURE_USE_REF',
            ],
        ];

        $combinable = [
            ast\AST_METHOD => $function_modifiers,
            ast\AST_FUNC_DECL => $function_modifiers,
            ast\AST_CLOSURE => $function_modifiers,
            ast\AST_ARROW_FUNC => $function_modifiers,
            ast\AST_CLASS_CONST_DECL => [
                flags\MODIFIER_PUBLIC => 'MODIFIER_PUBLIC',
                flags\MODIFIER_PROTECTED => 'MODIFIER_PROTECTED',
                flags\MODIFIER_PRIVATE => 'MODIFIER_PRIVATE',
            ],
            ast\AST_PROP_GROUP => $property_modifiers,
            ast\AST_TRAIT_ALIAS => $property_modifiers,
            ast\AST_DIM => [
                flags\DIM_ALTERNATIVE_SYNTAX => 'DIM_ALTERNATIVE_SYNTAX',
            ],
            ast\AST_CONDITIONAL => [
                flags\PARENTHESIZED_CONDITIONAL => 'PARENTHESIZED_CONDITIONAL',
            ],
            ast\AST_PARAM => [
                flags\PARAM_REF => 'PARAM_REF',
                flags\PARAM_VARIADIC => 'PARAM_VARIADIC',
            ],
        ];
    }

    return [$exclusive, $combinable];
}

/**
 * Computes a string representation of AST node flags such as
 * 'ASSIGN_DIV|TYPE_ARRAY'
 */
function format_flags(int $kind, int $flags) : string
{
    [$exclusive, $combinable] = get_flag_info();
    $flag_names = [];
    if (isset($exclusive[$kind])) {
        $flag_info = $exclusive[$kind];
        if (isset($flag_info[$flags])) {
            $flag_names[] = $flag_info[$flags];
        }
    } elseif (isset($combinable[$kind])) {
        $flag_info = $combinable[$kind];
        foreach ($flag_info as $flag => $name) {
            if ($flags & $flag) {
                $flag_names[] = $name;
            }
        }
    }

    return \implode('|', $flag_names);
}

/**
 * Dumps abstract syntax tree
 * Source: https://github.com/nikic/php-ast/blob/master/util.php
 * @param Node|string|int|float|null $ast
 * @param int $options (AST_DUMP_*)
 */
function ast_dump($ast, int $options = 0) : string
{
    if ($ast instanceof Node) {
        // $kind can be invalid for placeholder nodes or unexpected tolerant-php-parser classes
        $kind = $ast->kind;
        $result = is_int($kind) ? ast\get_kind_name($kind) : ("INVALID KIND: " . var_export($kind, true));

        if ($options & AST_DUMP_LINENOS) {
            $result .= " @ $ast->lineno";
            $end_lineno = $ast->endLineno ?? null;
            if (!\is_null($end_lineno)) {
                $result .= "-$end_lineno";
            }
        }

        if (is_int($kind) && ast\kind_uses_flags($kind)) {
            $flags = $ast->flags;
            if ($flags != 0) {
                $result .= "\n    flags: " . format_flags($kind, $flags);
            }
        }
        foreach ($ast->children as $i => $child) {
            $result .= "\n    $i: " . \str_replace("\n", "\n    ", ast_dump($child, $options));
        }
        return $result;
    } elseif ($ast === null) {
        return 'null';
    } elseif (\is_string($ast)) {
        return "\"$ast\"";
    } else {
        return (string) $ast;
    }
}
