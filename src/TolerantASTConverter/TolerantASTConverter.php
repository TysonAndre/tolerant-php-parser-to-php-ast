<?php declare(strict_types=1);
namespace TolerantASTConverter;

use Microsoft\PhpParser;
use Microsoft\PhpParser\Parser;
use Microsoft\PhpParser\DiagnosticsProvider;

/**
 * Source: https://github.com/TysonAndre/tolerant-php-parser-to-php-ast
 * Uses PhpParser to create an instance of \ast\Node.
 * Useful if the php-ast extension isn't actually installed.
 * @author Tyson Andre
 */
class TolerantASTConverter {
    // The latest stable version of php-ast.
    // For something > 40, update the library's release.
    // For something < 40, there are no releases.
    const AST_VERSION = 40;

    private static $should_add_placeholders = false;

    public static function set_should_add_placeholders(bool $value) : void {
        self::$should_add_placeholders = $value;
    }

    public static function ast_parse_code_fallback(string $source, int $version, bool $suppressErrors = false, array &$errors = null) {
        if ($version !== self::AST_VERSION) {
            throw new \InvalidArgumentException(sprintf("Unexpected version: want %d, got %d", self::AST_VERSION, $version));
        }
        // Aside: this can be implemented as a stub.
        $parserNode = self::phpparser_parse($source, $suppressErrors, $errors);
        return self::phpparser_to_phpast($parserNode, $version);
    }

    public static function phpparser_parse(string $source, bool $suppressErrors = false, array &$errors = null) {
        $parser = new Parser();  // TODO: Language version?
        // $nodeDumper = new PhpParser\NodeDumper();
        // TODO: Provide url
        $result = $parser->parseSourceFile($source);
        // TODO: extract errors?
        $errors = DiagnosticsProvider::getDiagnostics($result);
        return $result;
    }


    /**
     * @param PhpParser\Node|PhpParser\Node[] $parserNode
     * @param int $version
     */
    public static function phpparser_to_phpast($parserNode, int $version) {
        if ($version !== self::AST_VERSION) {
            throw new \InvalidArgumentException(sprintf("Unexpected version: want %d, got %d", self::AST_VERSION, $version));
        }
        if (is_array($parserNode)) {
            return self::_phpparser_stmtlist_to_ast_node($parserNode, 1);
        }
        return self::_phpparser_node_to_ast_node($parserNode);
    }

    private static function _phpparser_stmtlist_to_ast_node(array $parserNodes, ?int $lineno) : \ast\Node {
        $stmts = new \ast\Node();
        $stmts->kind = \ast\AST_STMT_LIST;
        $stmts->flags = 0;
        $children = [];
        foreach ($parserNodes as $parserNode) {
            $childNode = self::_phpparser_node_to_ast_node($parserNode);
            if (is_array($childNode)) {
                // Echo_ returns multiple children.
                foreach ($childNode as $childNodePart) {
                    $children[] = $childNodePart;
                }
            } else if (!is_null($childNode)) {
                $children[] = $childNode;
            }
        }
        if (!is_int($lineno)) {
            foreach ($parserNodes as $parserNode) {
                $childNodeLine = sl($parserNode);
                if ($childNodeLine > 0) {
                    $lineno = $childNodeLine;
                    break;
                }
            }
        }
        $stmts->lineno = $lineno ?? 0;
        $stmts->children = $children;
        return $stmts;
    }

    /**
     * @param PHPParser\Node[] $exprs
     */
    private static function _phpparser_expr_list_to_expr_list(array $exprs, int $lineno) : \ast\Node {
        $children = [];
        foreach ($exprs as $expr) {
            $childNode = self::_phpparser_node_to_ast_node($expr);
            if (is_array($childNode)) {
                // Echo_ returns multiple children.
                foreach ($childNode as $childNodePart) {
                    $children[] = $childNodePart;
                }
            } else if (!is_null($childNode)) {
                $children[] = $childNode;
            }
        }
        foreach ($exprs as $parserNode) {
            $childNodeLine = sl($parserNode);
            if ($childNodeLine > 0) {
                $lineno = $childNodeLine;
                break;
            }
        }
        return astnode(
            \ast\AST_EXPR_LIST,
            0,
            $children,
            $lineno
        );
    }

    /**
     * @param PhpParser\Node $n - The node from PHP-Parser
     * @return \ast\Node|\ast\Node[]|string|int|float|bool|null - whatever \ast\parse_code would return as the equivalent.
     * @suppress PhanUndeclaredProperty
     */
    private static final function _phpparser_node_to_ast_node($n) {
        if (!($n instanceof PhpParser\Node)) {
            debug_print_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS);
            throw new \InvalidArgumentException("Invalid type for node: " . (is_object($n) ? get_class($n) : gettype($n)));
        }

        static $callback_map;
        static $fallback_closure;
        if (\is_null($callback_map)) {
            $callback_map = self::_init_handle_map();
            $fallback_closure = function(PHPParser\Node $n, int $startLine) {
                return self::_ast_stub($n);
            };
        }
        $callback = $callback_map[get_class($n)] ?? $fallback_closure;
        return $callback($n, self::getStartLine($n));
    }

    private static function getStartLine(PhpParser\Node $n) : int {
        // TODO: binary search in an array mapping line number to character offset?
        // Currently returns character offset.
        return $n->getStart();
    }

    /**
     * This returns an array of values mapping class names to the closures which converts them to a scalar or \ast\Node or \ast\Node\Decl
     *
     * Why not a switch? Switches are slow until php 7.2, and there are dozens of class names to handle.
     *
     * - In php <= 7.1, the interpreter would loop through all possible cases, and compare against the value one by one.
     * - There are a lot of local variables to look at.
     *
     * @return Closure[]
     */
    private static function _init_handle_map() : array {
        $closures = [
            'Microsoft\PhpParser\Node\Expression\ArgumentExpression'                            => function(PhpParser\Node\Expression\ArgumentExpression $n, int $startLine) {
                // FIXME support foo(...$args)
                return self::_phpparser_node_to_ast_node($n->expression/*, $n->dotDotdotToken */);
            },
            'Microsoft\PhpParser\Node\Expression\ArrayCreationExpression'                    => function(PhpParser\Node\Expression\ArrayCreationExpression $n, int $startLine) : \ast\Node {
                return self::_phpparser_array_to_ast_array($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ArrayDimFetch'            => function(PhpParser\Node\Expression\ArrayDimFetch $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_DIM, 0, [
                    'expr' => self::_phpparser_node_to_ast_node($n->var),
                    'dim' => $n->dim !== null ? self::_phpparser_node_to_ast_node($n->dim) : null,
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Assign'                    => function(PhpParser\Node\Expression\Assign $n, int $startLine) : ?\ast\Node {
                return self::_ast_node_assign(
                    self::_phpparser_node_to_ast_node($n->var),
                    self::_phpparser_node_to_ast_node($n->expr),
                    $startLine,
                    false
                );
            },
            'Microsoft\PhpParser\Node\Expression\AssignRef'                 => function(PhpParser\Node\Expression\AssignRef $n, int $startLine) : ?\ast\Node {
                return self::_ast_node_assign(
                    self::_phpparser_node_to_ast_node($n->var),
                    self::_phpparser_node_to_ast_node($n->expr),
                    $startLine,
                    true
                );
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\BitwiseAnd'       => function(PhpParser\Node\Expression\AssignOp\BitwiseAnd $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_BITWISE_AND, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\BitwiseXor'       => function(PhpParser\Node\Expression\AssignOp\BitwiseXor $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_BITWISE_XOR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\BitwiseOr'        => function(PhpParser\Node\Expression\AssignOp\BitwiseOr $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_BITWISE_OR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Concat'           => function(PhpParser\Node\Expression\AssignOp\Concat $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_CONCAT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Div'           => function(PhpParser\Node\Expression\AssignOp\Div $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_DIV, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Mod'           => function(PhpParser\Node\Expression\AssignOp\Mod $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_MOD, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Mul'           => function(PhpParser\Node\Expression\AssignOp\Mul $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_MUL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Minus'           => function(PhpParser\Node\Expression\AssignOp\Minus $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_SUB, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Plus'           => function(PhpParser\Node\Expression\AssignOp\Plus $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_ADD, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\Pow'           => function(PhpParser\Node\Expression\AssignOp\Pow $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_POW, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\ShiftLeft'           => function(PhpParser\Node\Expression\AssignOp\ShiftLeft $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_SHIFT_LEFT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignOp\ShiftRight'           => function(PhpParser\Node\Expression\AssignOp\ShiftRight $n, int $startLine) : \ast\Node {
                return self::_ast_node_assignop(\ast\flags\BINARY_SHIFT_RIGHT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\BitwiseAnd' => function(PhpParser\Node\Expression\BinaryOp\BitwiseAnd $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BITWISE_AND, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\BitwiseOr' => function(PhpParser\Node\Expression\BinaryOp\BitwiseOr $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BITWISE_OR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\BitwiseXor' => function(PhpParser\Node\Expression\BinaryOp\BitwiseXor $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BITWISE_XOR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\BooleanAnd' => function(PhpParser\Node\Expression\BinaryOp\BooleanAnd $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BOOL_AND, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\BooleanOr' => function(PhpParser\Node\Expression\BinaryOp\BooleanOr $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BOOL_OR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Coalesce' => function(PhpParser\Node\Expression\BinaryOp\Coalesce $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_COALESCE, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Concat' => function(PhpParser\Node\Expression\BinaryOp\Concat $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_CONCAT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Div' => function(PhpParser\Node\Expression\BinaryOp\Div $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_DIV, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Equal' => function(PhpParser\Node\Expression\BinaryOp\Equal $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_EQUAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Greater' => function(PhpParser\Node\Expression\BinaryOp\Greater $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_GREATER, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\GreaterOrEqual' => function(PhpParser\Node\Expression\BinaryOp\GreaterOrEqual $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_GREATER_OR_EQUAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Identical' => function(PhpParser\Node\Expression\BinaryOp\Identical $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_IDENTICAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\LogicalAnd' => function(PhpParser\Node\Expression\BinaryOp\LogicalAnd $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BOOL_AND, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\LogicalOr' => function(PhpParser\Node\Expression\BinaryOp\LogicalOr $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BOOL_OR, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\LogicalXor' => function(PhpParser\Node\Expression\BinaryOp\LogicalXor $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_BOOL_XOR, $n, $startLine);
            },
            // FIXME: rest of binary operations.
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Minus' => function(PhpParser\Node\Expression\BinaryOp\Minus $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_SUB, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Mod' => function(PhpParser\Node\Expression\BinaryOp\Mod $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_MOD, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Mul' => function(PhpParser\Node\Expression\BinaryOp\Mul $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_MUL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\NotEqual' => function(PhpParser\Node\Expression\BinaryOp\NotEqual $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_NOT_EQUAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\NotIdentical' => function(PhpParser\Node\Expression\BinaryOp\NotIdentical $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_NOT_IDENTICAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Plus' => function(PhpParser\Node\Expression\BinaryOp\Plus $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_ADD, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Pow' => function(PhpParser\Node\Expression\BinaryOp\Pow $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_POW, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\ShiftLeft' => function(PhpParser\Node\Expression\BinaryOp\ShiftLeft $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_SHIFT_LEFT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\ShiftRight' => function(PhpParser\Node\Expression\BinaryOp\ShiftRight $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_SHIFT_RIGHT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Smaller' => function(PhpParser\Node\Expression\BinaryOp\Smaller $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_SMALLER, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\SmallerOrEqual' => function(PhpParser\Node\Expression\BinaryOp\SmallerOrEqual $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_IS_SMALLER_OR_EQUAL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BinaryOp\Spaceship' => function(PhpParser\Node\Expression\BinaryOp\Spaceship $n, int $startLine) : \ast\Node {
                return self::_ast_node_binaryop(\ast\flags\BINARY_SPACESHIP, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BitwiseNot' => function(PhpParser\Node\Expression\BitwiseNot $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_BITWISE_NOT, self::_phpparser_node_to_ast_node($n->expr), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\BooleanNot' => function(PhpParser\Node\Expression\BooleanNot $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_BOOL_NOT, self::_phpparser_node_to_ast_node($n->expr), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\ArrayCreationExpression' => function(PhpParser\Node\Expression\Cast\ArrayCreationExpression $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_ARRAY, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\Bool_' => function(PhpParser\Node\Expression\Cast\Bool_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_BOOL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\Double' => function(PhpParser\Node\Expression\Cast\Double $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_DOUBLE, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\Int_' => function(PhpParser\Node\Expression\Cast\Int_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_LONG, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\Object_' => function(PhpParser\Node\Expression\Cast\Object_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_OBJECT, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\String_' => function(PhpParser\Node\Expression\Cast\String_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_STRING, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Cast\Unset_' => function(PhpParser\Node\Expression\Cast\Unset_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_cast(\ast\flags\TYPE_NULL, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Closure' => function(PhpParser\Node\Expression\Closure $n, int $startLine) : \ast\Node {
                // TODO: is there a corresponding flag for $n->static? $n->byRef?
                return self::_ast_decl_closure(
                    $n->byRef,
                    $n->static,
                    self::_phpparser_params_to_ast_params($n->params, $startLine),
                    self::_phpparser_closure_uses_to_ast_closure_uses($n->uses, $startLine),
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    self::_phpparser_type_to_ast_node($n->returnType, sl($n->returnType) ?: $startLine),
                    $startLine,
                    $n->getAttribute('endLine'),
                    self::_extract_phpdoc_comment($n->getAttribute('comments'))
                );
                // FIXME: add a test of ClassConstFetch to php-ast
            },
            'Microsoft\PhpParser\Node\Expression\ClassConstFetch' => function(PhpParser\Node\Expression\ClassConstFetch $n, int $startLine) : ?\ast\Node {
                return self::_phpparser_classconstfetch_to_ast_classconstfetch($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Clone_' => function(PhpParser\Node\Expression\Clone_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_CLONE, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expr)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ConstFetch' => function(PhpParser\Node\Expression\ConstFetch $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_CONST, 0, ['name' => self::_phpparser_node_to_ast_node($n->name)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ErrorSuppress' => function(PhpParser\Node\Expression\ErrorSuppress $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_SILENCE, self::_phpparser_node_to_ast_node($n->expr), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Empty_' => function(PhpParser\Node\Expression\Empty_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_EMPTY, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expr)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Eval_' => function(PhpParser\Node\Expression\Eval_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_eval(
                    self::_phpparser_node_to_ast_node($n->expr),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\Error' => function(PhpParser\Node\Expression\Error $n, int $startLine) {
                // This is where PhpParser couldn't parse a node.
                // TODO: handle this.
                return null;
            },
            'Microsoft\PhpParser\Node\Expression\Exit_' => function(PhpParser\Node\Expression\Exit_ $n, int $startLine) {
                return astnode(\ast\AST_EXIT, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expr)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\FuncCall' => function(PhpParser\Node\Expression\FuncCall $n, int $startLine) : \ast\Node {
                return self::_ast_node_call(
                    self::_phpparser_node_to_ast_node($n->name),
                    self::_phpparser_arg_list_to_ast_arg_list($n->args, $startLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\Include_' => function(PhpParser\Node\Expression\Include_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_include(
                    self::_phpparser_node_to_ast_node($n->expr),
                    $startLine,
                    $n->type
                );
            },
            'Microsoft\PhpParser\Node\Expression\Instanceof_' => function(PhpParser\Node\Expression\Instanceof_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_INSTANCEOF, 0, [
                    'expr'  => self::_phpparser_node_to_ast_node($n->expr),
                    'class' => self::_phpparser_node_to_ast_node($n->class),
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Isset_' => function(PhpParser\Node\Expression\Isset_ $n, int $startLine) : \ast\Node {
                $astIssets = [];
                foreach ($n->vars as $var) {
                    $astIssets[] = astnode(\ast\AST_ISSET, 0, [
                        'var' => self::_phpparser_node_to_ast_node($var),
                    ], $startLine);
                }
                $e = $astIssets[0];
                for ($i = 1; $i < \count($astIssets); $i++) {
                    $right = $astIssets[$i];
                    $e = astnode(
                        \ast\AST_BINARY_OP,
                        \ast\flags\BINARY_BOOL_AND,
                        [
                            'left' => $e,
                            'right' => $right,
                        ],
                        $e->lineno
                    );
                }
                return $e;
            },
            'Microsoft\PhpParser\Node\Expression\List_' => function(PhpParser\Node\Expression\List_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_list_to_ast_list($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\MethodCall' => function(PhpParser\Node\Expression\MethodCall $n, int $startLine) : \ast\Node {
                return self::_ast_node_method_call(
                    self::_phpparser_node_to_ast_node($n->var),
                    is_string($n->name) ? $n->name : self::_phpparser_node_to_ast_node($n->name),
                    self::_phpparser_arg_list_to_ast_arg_list($n->args, $startLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\New_' => function(PhpParser\Node\Expression\New_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_NEW, 0, [
                    'class' => self::_phpparser_node_to_ast_node($n->class),
                    'args' => self::_phpparser_arg_list_to_ast_arg_list($n->args, $startLine),
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PreInc' => function(PhpParser\Node\Expression\PreInc $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_PRE_INC, 0, ['var' => self::_phpparser_node_to_ast_node($n->var)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PreDec' => function(PhpParser\Node\Expression\PreDec $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_PRE_DEC, 0, ['var' => self::_phpparser_node_to_ast_node($n->var)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PostInc' => function(PhpParser\Node\Expression\PostInc $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_POST_INC, 0, ['var' => self::_phpparser_node_to_ast_node($n->var)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PostDec' => function(PhpParser\Node\Expression\PostDec $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_POST_DEC, 0, ['var' => self::_phpparser_node_to_ast_node($n->var)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Print_' => function(PhpParser\Node\Expression\Print_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_PRINT,
                    0,
                    ['expr' => self::_phpparser_node_to_ast_node($n->expr)],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\PropertyFetch' => function(PhpParser\Node\Expression\PropertyFetch $n, int $startLine) : ?\ast\Node {
                return self::_phpparser_propertyfetch_to_ast_prop($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ShellExec' => function(PhpParser\Node\Expression\ShellExec $n, int $startLine) : \ast\Node {
                $parts = $n->parts;
                if (\count($parts) === 1 && $parts[0] instanceof PhpParser\Node\Scalar\EncapsedStringPart) {
                    $value = $parts[0]->value;
                } else {
                    $value_inner = array_map(function(PhpParser\Node $node) { return self::_phpparser_node_to_ast_node($node); }, $parts);
                    $value = astnode(\ast\AST_ENCAPS_LIST, 0, $value_inner, $startLine);
                }
                return astnode(\ast\AST_SHELL_EXEC, 0, ['expr' => $value], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\StaticCall' => function(PhpParser\Node\Expression\StaticCall $n, int $startLine) : \ast\Node {
                return self::_ast_node_static_call(
                    self::_phpparser_node_to_ast_node($n->class),
                    is_string($n->name) ? $n->name : self::_phpparser_node_to_ast_node($n->name),
                    self::_phpparser_arg_list_to_ast_arg_list($n->args, $startLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\StaticPropertyFetch' => function(PhpParser\Node\Expression\StaticPropertyFetch $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_STATIC_PROP,
                    0,
                    [
                        'class' => self::_phpparser_node_to_ast_node($n->class),
                        'prop' => is_string($n->name) ? $n->name : self::_phpparser_node_to_ast_node($n->name),
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\Ternary' => function(PhpParser\Node\Expression\Ternary $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_CONDITIONAL,
                    0,
                    [
                        'cond' => self::_phpparser_node_to_ast_node($n->cond),
                        'true' => $n->if !== null ? self::_phpparser_node_to_ast_node($n->if) : null,
                        'false' => self::_phpparser_node_to_ast_node($n->else),
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\UnaryMinus' => function(PhpParser\Node\Expression\UnaryMinus $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_MINUS, self::_phpparser_node_to_ast_node($n->expr), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\UnaryPlus' => function(PhpParser\Node\Expression\UnaryPlus $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_PLUS, self::_phpparser_node_to_ast_node($n->expr), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Variable' => function(PhpParser\Node\Expression\Variable $n, int $startLine) : ?\ast\Node {
                return self::_ast_node_variable($n->name, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\Yield_' => function(PhpParser\Node\Expression\Yield_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_YIELD,
                    0,
                    [
                        'value' => $n->value !== null ? self::_phpparser_node_to_ast_node($n->value) : null,
                        'key'   => $n->key   !== null ? self::_phpparser_node_to_ast_node($n->key) : null,
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\YieldFrom' => function(PhpParser\Node\Expression\YieldFrom $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_YIELD_FROM,
                    0,
                    ['expr' => self::_phpparser_node_to_ast_node($n->expr)],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Name' => function(PhpParser\Node\Name $n, int $startLine) : \ast\Node {
                return self::_ast_node_name(
                    self::_phpparser_name_to_string($n),
                    $startLine
                );
            },
            /**
             * @suppress PhanDeprecatedProperty TODO: figure out alternative
             */
            'Microsoft\PhpParser\Node\Name\FullyQualified' => function(PhpParser\Node\Name\FullyQualified $n, int $startLine) : \ast\Node {
                return self::_ast_node_name_fullyqualified(
                    implode('\\', $n->parts),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\NullableType' => function(PhpParser\Node\NullableType $n, int $startLine) : \ast\Node {
                return self::_ast_node_nullable_type(
                    self::_phpparser_type_to_ast_node($n->type, $startLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Param' => function(PhpParser\Node\Param $n, int $startLine) : \ast\Node {
                $typeLine = sl($n->type) ?: $startLine;
                $defaultLine = sl($n->default) ?: $typeLine;
                return self::_ast_node_param(
                    $n->byRef,
                    $n->variadic,
                    self::_phpparser_type_to_ast_node($n->type, $typeLine),
                    $n->name,
                    self::_phpparser_type_to_ast_node($n->default, $defaultLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Scalar\Encapsed' => function(PhpParser\Node\Scalar\Encapsed $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_ENCAPS_LIST,
                    0,
                    array_map(function(PhpParser\Node $n) { return self::_phpparser_node_to_ast_node($n); }, $n->parts),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Scalar\EncapsedStringPart' => function(PhpParser\Node\Scalar\EncapsedStringPart $n, int $startLine) : string {
                return $n->value;
            },
            'Microsoft\PhpParser\Node\Scalar\DNumber' => function(PhpParser\Node\Scalar\DNumber $n, int $startLine) : float {
                return (float)$n->value;
            },
            'Microsoft\PhpParser\Node\Scalar\LNumber' => function(PhpParser\Node\Scalar\LNumber $n, int $startLine) : int {
                return (int)$n->value;
            },
            'Microsoft\PhpParser\Node\Scalar\String_' => function(PhpParser\Node\Scalar\String_ $n, int $startLine) : string {
                return (string)$n->value;
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Class_' => function(PhpParser\Node\Scalar\MagicConst\Class_ $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_CLASS, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Dir' => function(PhpParser\Node\Scalar\MagicConst\Dir $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_DIR, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\File' => function(PhpParser\Node\Scalar\MagicConst\File $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_FILE, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Function_' => function(PhpParser\Node\Scalar\MagicConst\Function_ $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_FUNCTION, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Line' => function(PhpParser\Node\Scalar\MagicConst\Line $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_LINE, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Method' => function(PhpParser\Node\Scalar\MagicConst\Method $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_METHOD, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Namespace_' => function(PhpParser\Node\Scalar\MagicConst\Namespace_ $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_NAMESPACE, $startLine);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Trait_' => function(PhpParser\Node\Scalar\MagicConst\Trait_ $n, int $startLine) : \ast\Node {
                return self::_ast_magic_const(\ast\flags\MAGIC_TRAIT, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Break_' => function(PhpParser\Node\Stmt\Break_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_BREAK, 0, ['depth' => isset($n->num) ? self::_phpparser_node_to_ast_node($n->num) : null], $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Catch_' => function(PhpParser\Node\Stmt\Catch_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_catch(
                    self::_phpparser_name_list_to_ast_name_list($n->types, $startLine),
                    $n->var,
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Class_' => function(PhpParser\Node\Stmt\Class_ $n, int $startLine) : \ast\Node {
                $endLine = $n->getAttribute('endLine') ?: $startLine;
                return self::_ast_stmt_class(
                    self::_phpparser_class_flags_to_ast_class_flags($n->flags),
                    $n->name,
                    $n->extends,
                    $n->implements ?: null,
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine,
                    $endLine,
                    self::_extract_phpdoc_comment($n->getAttribute('comments'))
                );
            },
            'Microsoft\PhpParser\Node\Stmt\ClassConst' => function(PhpParser\Node\Stmt\ClassConst $n, int $startLine) : \ast\Node {
                return self::_phpparser_class_const_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\ClassMethod' => function(PhpParser\Node\Stmt\ClassMethod $n, int $startLine) : \ast\Node {
                return astdecl(
                    \ast\AST_METHOD,
                    self::_phpparser_visibility_to_ast_visibility($n->flags) | ($n->byRef ? \ast\flags\RETURNS_REF : 0),
                    [
                        'params' => self::_phpparser_params_to_ast_params($n->params, $startLine),
                        'uses' => null,  // TODO: anonymous class?
                        'stmts' => is_array($n->stmts) ? self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine) : null,
                        'returnType' => self::_phpparser_type_to_ast_node($n->returnType, sl($n->returnType) ?: $startLine)
                    ],
                    $startLine,
                    self::_extract_phpdoc_comment($n->getAttribute('comments')),
                    $n->name,
                    $n->getAttribute('endLine')
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Const_' => function(PhpParser\Node\Stmt\Const_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_const_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Continue_' => function(PhpParser\Node\Stmt\Continue_ $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_CONTINUE, 0, ['depth' => isset($n->num) ? self::_phpparser_node_to_ast_node($n->num) : null], $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Declare_' => function(PhpParser\Node\Stmt\Declare_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_declare(
                    self::_phpparser_declare_list_to_ast_declares($n->declares, $startLine),
                    is_array($n->stmts) ? self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine) : null,
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Do_' => function(PhpParser\Node\Stmt\Do_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_do_while(
                    self::_phpparser_node_to_ast_node($n->cond),
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine
                );
            },
            /**
             * @return \ast\Node|\ast\Node[]
             */
            'Microsoft\PhpParser\Node\Stmt\Echo_' => function(PhpParser\Node\Stmt\Echo_ $n, int $startLine) {
                $astEchos = [];
                foreach ($n->exprs as $expr) {
                    $astEchos[] = self::_ast_stmt_echo(
                        self::_phpparser_node_to_ast_node($expr),
                        $startLine
                    );
                }
                return count($astEchos) === 1 ? $astEchos[0] : $astEchos;
            },
            'Microsoft\PhpParser\Node\Stmt\Foreach_' => function(PhpParser\Node\Stmt\Foreach_ $n, int $startLine) : \ast\Node {
                $value = self::_phpparser_node_to_ast_node($n->valueVar);
                if ($n->byRef) {
                    $value = astnode(
                        \ast\AST_REF,
                        0,
                        ['var' => $value],
                        $value->lineno ?? $startLine
                    );
                }
                return astnode(
                    \ast\AST_FOREACH,
                    0,
                    [
                        'expr' => self::_phpparser_node_to_ast_node($n->expr),
                        'value' => $value,
                        'key' => $n->keyVar !== null ? self::_phpparser_node_to_ast_node($n->keyVar) : null,
                        'stmts' => self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    ],
                    $startLine
                );
                //return self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Finally_' => function(PhpParser\Node\Stmt\Finally_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Function_' => function(PhpParser\Node\Stmt\Function_ $n, int $startLine) : \ast\Node {
                $endLine = $n->getAttribute('endLine') ?: $startLine;
                $returnType = $n->returnType;
                $returnTypeLine = sl($returnType) ?: $endLine;
                $astReturnType = self::_phpparser_type_to_ast_node($returnType, $returnTypeLine);

                return self::_ast_decl_function(
                    $n->byRef,
                    $n->name,
                    self::_phpparser_params_to_ast_params($n->params, $startLine),
                    null,  // uses
                    self::_phpparser_type_to_ast_node($returnType, $returnTypeLine),
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine,
                    $endLine,
                    self::_extract_phpdoc_comment($n->getAttribute('comments'))
                );
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Stmt\Global_' => function(PhpParser\Node\Stmt\Global_ $n, int $startLine) {
                $globalNodes = [];
                foreach ($n->vars as $var) {
                    $globalNodes[] = astnode(\ast\AST_GLOBAL, 0, ['var' => self::_phpparser_node_to_ast_node($var)], sl($var) ?: $startLine);
                }
                return \count($globalNodes) === 1 ? $globalNodes[0] : $globalNodes;
            },
            'Microsoft\PhpParser\Node\Stmt\If_' => function(PhpParser\Node\Stmt\If_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_if_stmt_to_ast_if_stmt($n);
            },
            'Microsoft\PhpParser\Node\Stmt\InlineHTML' => function(PhpParser\Node\Stmt\InlineHTML $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_echo($n->value, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Interface_' => function(PhpParser\Node\Stmt\Interface_ $n, int $startLine) : \ast\Node {
                $endLine = $n->getAttribute('endLine') ?: $startLine;
                return self::_ast_stmt_class(
                    \ast\flags\CLASS_INTERFACE,
                    $n->name,
                    null,
                    null,
                    is_array($n->stmts) ? self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine) : null,
                    $startLine,
                    $endLine,
                    self::_extract_phpdoc_comment($n->getAttribute('comments'))
                );
            },
            'Microsoft\PhpParser\Node\Stmt\For_' => function(PhpParser\Node\Stmt\For_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_FOR,
                    0,
                    [
                        'init' => \count($n->init) > 0 ? self::_phpparser_expr_list_to_expr_list($n->init, $startLine) : null,
                        'cond' => \count($n->cond) > 0 ? self::_phpparser_expr_list_to_expr_list($n->cond, $startLine) : null,
                        'loop' => \count($n->loop) > 0 ? self::_phpparser_expr_list_to_expr_list($n->loop, $startLine) : null,
                        'stmts' => self::_phpparser_stmtlist_to_ast_node($n->stmts ?? [], $startLine),
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\GroupUse' => function(PhpParser\Node\Stmt\GroupUse $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_group_use(
                    $n->type,
                    self::_phpparser_name_to_string($n->prefix),
                    self::_phpparser_use_list_to_ast_use_list($n->uses),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Namespace_' => function(PhpParser\Node\Stmt\Namespace_ $n, int $startLine) : \ast\Node {
                $nodeDumper = new PhpParser\NodeDumper([
                    'dumpComments' => true,
                    'dumpPositions' => true,
                ]);
                return astnode(
                    \ast\AST_NAMESPACE,
                    0,
                    [
                        'name' => $n->name !== null ? self::_phpparser_name_to_string($n->name) : null,
                        'stmts' => isset($n->stmts) ? self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine) : null,
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Nop' => function(PhpParser\Node\Stmt\Nop $n, int $startLine) : array {
                // `;;`
                return [];
            },
            'Microsoft\PhpParser\Node\Stmt\Property' => function(PhpParser\Node\Stmt\Property $n, int $startLine) : \ast\Node {
                return self::_phpparser_property_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\Return_' => function(PhpParser\Node\Stmt\Return_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_return($n->expr !== null ? self::_phpparser_node_to_ast_node($n->expr) : null, $startLine);
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Stmt\Static_' => function(PhpParser\Node\Stmt\Static_ $n, int $startLine) {
                $staticNodes = [];
                foreach ($n->vars as $var) {
                    $staticNodes[] = astnode(\ast\AST_STATIC, 0, [
                        'var' => astnode(\ast\AST_VAR, 0, ['name' => $var->name], sl($var) ?: $startLine),
                        'default' => $var->default !== null ? self::_phpparser_node_to_ast_node($var->default) : null,
                    ], sl($var) ?: $startLine);
                }
                return \count($staticNodes) === 1 ? $staticNodes[0] : $staticNodes;
            },
            'Microsoft\PhpParser\Node\Stmt\Switch_' => function(PhpParser\Node\Stmt\Switch_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_switch_list_to_ast_switch($n);
            },
            'Microsoft\PhpParser\Node\Stmt\Throw_' => function(PhpParser\Node\Stmt\Throw_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_THROW,
                    0,
                    ['expr' => self::_phpparser_node_to_ast_node($n->expr)],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\Trait_' => function(PhpParser\Node\Stmt\Trait_ $n, int $startLine) : \ast\Node {
                $endLine = $n->getAttribute('endLine') ?: $startLine;
                return self::_ast_stmt_class(
                    \ast\flags\CLASS_TRAIT,
                    $n->name,
                    null,
                    null,
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine,
                    $endLine,
                    self::_extract_phpdoc_comment($n->getAttribute('comments'))
                );
            },
            'Microsoft\PhpParser\Node\Stmt\TraitUse' => function(PhpParser\Node\Stmt\TraitUse $n, int $startLine) : \ast\Node {
                if (\is_array($n->adaptations) && \count($n->adaptations) > 0) {
                    $adaptations_inner = array_map(function(PhpParser\Node\Stmt\TraitUseAdaptation $n) : \ast\Node {
                        return self::_phpparser_node_to_ast_node($n);
                    }, $n->adaptations);
                    $adaptations = astnode(\ast\AST_TRAIT_ADAPTATIONS, 0, $adaptations_inner, $adaptations_inner[0]->lineno ?: $startLine);
                } else {
                    $adaptations = null;
                }
                return astnode(
                    \ast\AST_USE_TRAIT,
                    0,
                    [
                        'traits' => self::_phpparser_name_list_to_ast_name_list($n->traits, $startLine),
                        'adaptations' => $adaptations,
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\TraitUseAdaptation\Alias' => function(PhpParser\Node\Stmt\TraitUseAdaptation\Alias $n, int $startLine) : \ast\Node {
                $old_class = $n->trait !== null ? self::_phpparser_node_to_ast_node($n->trait) : null;
                $flags = ($n->trait instanceof PhpParser\Node\Name\FullyQualified) ? \ast\flags\NAME_FQ : \ast\flags\NAME_NOT_FQ;
                // TODO: flags for visibility
                return astnode(\ast\AST_TRAIT_ALIAS, self::_phpparser_visibility_to_ast_visibility($n->newModifier ?? 0, false), [
                    'method' => astnode(\ast\AST_METHOD_REFERENCE, 0, [
                        'class' => $old_class,
                        'method' => $n->method,
                    ], $startLine),
                    'alias' => $n->newName,
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\TraitUseAdaptation\Precedence' => function(PhpParser\Node\Stmt\TraitUseAdaptation\Precedence $n, int $startLine) : \ast\Node {
                $old_class = $n->trait !== null ? self::_phpparser_name_to_string($n->trait) : null;
                $flags = ($n->trait instanceof PhpParser\Node\Name\FullyQualified) ? \ast\flags\NAME_FQ : \ast\flags\NAME_NOT_FQ;
                // TODO: flags for visibility
                return astnode(\ast\AST_TRAIT_PRECEDENCE, 0, [
                    'method' => astnode(\ast\AST_METHOD_REFERENCE, 0, [
                        'class' => astnode(\ast\AST_NAME, $flags, ['name' => $old_class], $startLine),
                        'method' => $n->method,
                    ], $startLine),
                    'insteadof' => self::_phpparser_name_list_to_ast_name_list($n->insteadof, $startLine),
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Stmt\TryCatch' => function(PhpParser\Node\Stmt\TryCatch $n, int $startLine) : \ast\Node {
                if (!is_array($n->catches)) {
                    throw new \Error(sprintf("Unsupported type %s\n%s", get_class($n), var_export($n->catches, true)));
                }
                return self::_ast_node_try(
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine), // $n->try
                    self::_phpparser_catchlist_to_ast_catchlist($n->catches, $startLine),
                    isset($n->finally) ? self::_phpparser_stmtlist_to_ast_node($n->finally->stmts, sl($n->finally)) : null,
                    $startLine
                );
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Stmt\Unset_' => function(PhpParser\Node\Stmt\Unset_ $n, int $startLine) {
                $stmts = [];
                foreach ($n->vars as $var) {
                    $stmts[] = astnode(\ast\AST_UNSET, 0, ['var' => self::_phpparser_node_to_ast_node($var)], sl($var) ?: $startLine);
                }
                return \count($stmts) === 1 ? $stmts[0] : $stmts;
            },
            'Microsoft\PhpParser\Node\Stmt\Use_' => function(PhpParser\Node\Stmt\Use_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_use(
                    $n->type,
                    self::_phpparser_use_list_to_ast_use_list($n->uses),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Stmt\While_' => function(PhpParser\Node\Stmt\While_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_while(
                    self::_phpparser_node_to_ast_node($n->cond),
                    self::_phpparser_stmtlist_to_ast_node($n->stmts, $startLine),
                    $startLine
                );
            },
        ];

        foreach ($closures as $key => $value) {
            assert(class_exists($key), "Class $key should exist");
        }
        return $closures;
    }

    private static function _ast_node_try(
        $tryNode,
        $catchesNode,
        $finallyNode,
        int $startLine
    ) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_TRY;
        $node->flags = 0;
        $node->lineno = $startLine;
        $children = [
            'try' => $tryNode,
        ];
        if ($catchesNode !== null) {
            $children['catches'] = $catchesNode;
        }
        $children['finally'] = $finallyNode;
        $node->children = $children;
        return $node;
    }

    // FIXME types
    private static function _ast_stmt_catch($types, string $var, $stmts, int $lineno) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_CATCH;
        $node->lineno = $lineno;
        $node->flags = 0;
        $node->children = [
            'class' => $types,
            'var' => astnode(\ast\AST_VAR, 0, ['name' => $var], end($types->children)->lineno),  // FIXME AST_VAR
            'stmts' => $stmts,
        ];
        return $node;
    }

    private static function _phpparser_catchlist_to_ast_catchlist(array $catches, int $lineno) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_CATCH_LIST;
        $node->flags = 0;
        $children = [];
        foreach ($catches as $parserCatch) {
            $children[] = self::_phpparser_node_to_ast_node($parserCatch);
        }
        $node->lineno = $children[0]->lineno ?? $lineno;
        $node->children = $children;
        return $node;
    }

    private static function _phpparser_name_list_to_ast_name_list(array $types, int $line) : \ast\Node {
        $astTypes = [];
        foreach ($types as $type) {
            $astTypes[] = self::_phpparser_node_to_ast_node($type);
        }
        return astnode(\ast\AST_NAME_LIST, 0, $astTypes, $line);
    }

    private static function _ast_node_while($cond, $stmts, int $startLine) : \ast\Node {
        return astnode(
            \ast\AST_WHILE,
            0,
            [
                'cond' => $cond,
                'stmts' => $stmts,
            ],
            $startLine
        );
    }

    private static function _ast_node_do_while($cond, $stmts, int $startLine) : \ast\Node {
        return astnode(
            \ast\AST_DO_WHILE,
            0,
            [
                'stmts' => $stmts,
                'cond' => $cond,
            ],
            $startLine
        );
    }

    private static function _ast_node_assign($var, $expr, int $line, bool $ref) : ?\ast\Node {
        if ($expr === null) {
            if (self::$should_add_placeholders) {
                $expr = '__INCOMPLETE_EXPR__';
            } else {
                return null;
            }
        }
        $node = new \ast\Node();
        $node->kind = $ref ? \ast\AST_ASSIGN_REF : \ast\AST_ASSIGN;
        $node->flags = 0;
        $node->children = [
            'var'  => $var,
            'expr' => $expr,
        ];
        $node->lineno = $line;
        return $node;
    }

    private static function _ast_node_unary_op(int $flags, $expr, int $line) : \ast\Node {
        return astnode(\ast\AST_UNARY_OP, $flags, ['expr' => $expr], $line);
    }

    private static function _ast_node_cast(int $flags, PhpParser\Node\Expression\Cast $n, int $line) : \ast\Node {
        return astnode(\ast\AST_CAST, $flags, ['expr' => self::_phpparser_node_to_ast_node($n->expr)], sl($n) ?: $line);
    }

    private static function _ast_node_eval($expr, int $line) : \ast\Node {
        return astnode(\ast\AST_INCLUDE_OR_EVAL, \ast\flags\EXEC_EVAL, ['expr' => $expr], $line);
    }

    private static function _phpparser_include_flags_to_ast_include_flags(int $type) : int {
        switch($type) {
        case PhpParser\Node\Expression\Include_::TYPE_INCLUDE:
            return \ast\flags\EXEC_INCLUDE;
        case PhpParser\Node\Expression\Include_::TYPE_INCLUDE_ONCE:
            return \ast\flags\EXEC_INCLUDE_ONCE;
        case PhpParser\Node\Expression\Include_::TYPE_REQUIRE:
            return \ast\flags\EXEC_REQUIRE;
        case PhpParser\Node\Expression\Include_::TYPE_REQUIRE_ONCE:
            return \ast\flags\EXEC_REQUIRE_ONCE;
        default:
            throw new \Error("Unrecognized PhpParser include/require type $type");
        }
    }
    private static function _ast_node_include($expr, int $line, int $type) : \ast\Node {
        $flags = self::_phpparser_include_flags_to_ast_include_flags($type);
        return astnode(\ast\AST_INCLUDE_OR_EVAL, $flags, ['expr' => $expr], $line);
    }

    /**
     * @param PhpParser\Node\Name|string|null $type
     * @return \ast\Node|null
     */
    private static function _phpparser_type_to_ast_node($type, int $line) {
        if (is_null($type)) {
            return $type;
        }
        if (\is_string($type)) {
            switch(strtolower($type)) {
            case 'null':
                $flags = \ast\flags\TYPE_NULL; break;
            case 'bool':
                $flags = \ast\flags\TYPE_BOOL; break;
            case 'int':
                $flags = \ast\flags\TYPE_LONG; break;
            case 'float':
                $flags = \ast\flags\TYPE_DOUBLE; break;
            case 'string':
                $flags = \ast\flags\TYPE_STRING; break;
            case 'array':
                $flags = \ast\flags\TYPE_ARRAY; break;
            case 'object':
                $flags = \ast\flags\TYPE_OBJECT; break;
            case 'callable':
                $flags = \ast\flags\TYPE_CALLABLE; break;
            case 'void':
                $flags = \ast\flags\TYPE_VOID; break;
            case 'iterable':
                $flags = \ast\flags\TYPE_ITERABLE; break;
            default:
                $node = new \ast\Node();
                $node->kind = \ast\AST_NAME;
                $node->flags = substr($type, 0, 1) === '\\' ? \ast\flags\NAME_FQ : \ast\flags\NAME_NOT_FQ;  // FIXME wrong.
                $node->lineno = $line;
                $node->children = [
                    'name' => $type,
                ];
                return $node;
            }
            $node = new \ast\Node();
            $node->kind = \ast\AST_TYPE;
            $node->flags = $flags;
            $node->lineno = $line;
            $node->children = [];
            return $node;
        }
        return self::_phpparser_node_to_ast_node($type);
    }

    /**
     * @param bool $byRef
     * @param ?\ast\Node $type
     */
    private static function _ast_node_param(bool $byRef, bool $variadic, $type, $name, $default, int $line) : \ast\Node {
        $node = new \ast\Node;
        $node->kind = \ast\AST_PARAM;
        $node->flags = ($byRef ? \ast\flags\PARAM_REF : 0) | ($variadic ? \ast\flags\PARAM_VARIADIC : 0);
        $node->lineno = $line;
        $node->children = [
            'type' => $type,
            'name' => $name,
            'default' => $default,
        ];

        return $node;
    }

    private static function _ast_node_nullable_type(\ast\Node $type, int $line) {
        $node = new \ast\Node;
        $node->kind = \ast\AST_NULLABLE_TYPE;
        // FIXME: Why is this a special case in php-ast? (e.g. nullable int has no flags on the nullable node)
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['type' => $type];
        return $node;
    }

    private static function _ast_node_name(string $name, int $line) : \ast\Node {
        return astnode(\ast\AST_NAME, \ast\flags\NAME_NOT_FQ, ['name' => $name], $line);
    }

    private static function _ast_node_name_fullyqualified(string $name, int $line) : \ast\Node {
        return astnode(\ast\AST_NAME, \ast\flags\NAME_FQ, ['name' => $name], $line);
    }

    private static function _ast_node_variable($expr, int $line) : ?\ast\Node {
        // TODO: 2 different ways to handle an Error. 1. Add a placeholder. 2. remove all of the statements in that tree.
        if ($expr instanceof PhpParser\Node) {
            $expr = self::_phpparser_node_to_ast_node($expr);
            if ($expr === null) {
                if (self::$should_add_placeholders) {
                    $expr = '__INCOMPLETE_VARIABLE__';
                } else {
                    return null;
                }
            }
        }
        $node = new \ast\Node;
        $node->kind = \ast\AST_VAR;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['name' => $expr];
        return $node;
    }

    private static function _ast_magic_const(int $flags, int $line) {
        return astnode(\ast\AST_MAGIC_CONST, $flags, [], $line);
    }

    private static function _phpparser_params_to_ast_params(array $parserParams, int $line) : \ast\Node {
        $newParams = [];
        foreach ($parserParams as $parserNode) {
            $newParams[] = self::_phpparser_node_to_ast_node($parserNode);
        }
        $newParamsNode = new \ast\Node();
        $newParamsNode->kind = \ast\AST_PARAM_LIST;
        $newParamsNode->flags = 0;
        $newParamsNode->children = $newParams;
        $newParamsNode->lineno = $line;
        return $newParamsNode;
    }

    /**
     * @suppress PhanTypeMismatchProperty - Deliberately wrong type of kind
     */
    private static function _ast_stub($parserNode) : \ast\Node{
        // Debugging code.
        if (getenv('AST_THROW_INVALID')) {
            throw new \Error("TODO:" . get_class($parserNode));
        }

        $node = new \ast\Node();
        $node->kind = "TODO:" . get_class($parserNode);
        $node->flags = 0;
        $node->lineno = $parserNode->getAttribute('startLine');
        $node->children = null;
        return $node;
    }

    /**
     * @param PhpParser\Node\Expression\ClosureUse[] $uses
     * @param int $line
     * @return ?\ast\Node
     */
    private static function _phpparser_closure_uses_to_ast_closure_uses(
        array $uses,
        int $line
    ) {
        if (count($uses) === 0) {
            return null;
        }
        $astUses = [];
        foreach ($uses as $use) {
            $astUses[] = astnode(\ast\AST_CLOSURE_VAR, $use->byRef ? 1 : 0, ['name' => $use->var], $use->getAttribute('startLine'));
        }
        return astnode(\ast\AST_CLOSURE_USES, 0, $astUses, $astUses[0]->lineno ?? $line);

    }

    private static function _ast_decl_closure(
        bool $byRef,
        bool $static,
        \ast\Node $params,
        $uses,
        $stmts,
        $returnType,
        int $startLine,
        int $endLine,
        ?string $docComment
    ) : \ast\Node\Decl {
        $node = new \ast\Node\Decl;
        $node->kind = \ast\AST_CLOSURE;
        $node->flags = ($byRef ? \ast\flags\RETURNS_REF : 0) | ($static ? \ast\flags\MODIFIER_STATIC : 0);
        $node->lineno = $startLine;
        $node->endLineno = $endLine;
        if ($docComment) { $node->docComment = $docComment; }
        $node->children = [
            'params' => $params,
            'uses' => $uses,
            'stmts' => $stmts,
            'returnType' => $returnType,
        ];
        $node->name = '{closure}';
        return $node;
    }

    /**
     * @suppress PhanTypeMismatchProperty
     */
    private static function _ast_decl_function(
        bool $byRef,
        string $name,
        \ast\Node $params,
        ?array $uses,
        $returnType,
        $stmts,
        int $line,
        int $endLine,
        ?string $docComment
    ) : \ast\Node\Decl {
        $node = new \ast\Node\Decl;
        $node->kind = \ast\AST_FUNC_DECL;
        $node->flags = 0;
        $node->lineno = $line;
        $node->endLineno = $endLine;
        $node->children = [
            'params' => $params,
            'uses' => $uses,
            'stmts' => $stmts,
            'returnType' => $returnType,
        ];
        $node->name = $name;
        $node->docComment = $docComment;
        return $node;
    }

    private static function _phpparser_class_flags_to_ast_class_flags(int $flags) {
        $astFlags = 0;
        if ($flags & PhpParser\Node\Stmt\Class_::MODIFIER_ABSTRACT) {
            $astFlags |= \ast\flags\CLASS_ABSTRACT;
        }
        if ($flags & PhpParser\Node\Stmt\Class_::MODIFIER_FINAL) {
            $astFlags |= \ast\flags\CLASS_FINAL;
        }
        return $astFlags;
    }

    /**
     * @param int $flags
     * @param ?string $name
     * @param mixed|null $extends TODO
     * @param ?array $implements
     * @param ?\ast\Node $stmts
     * @param int $line
     * @param int $endLine
     * @suppress PhanTypeMismatchProperty (?string to string|null is incorrectly reported)
     */
    private static function _ast_stmt_class(
        int $flags,
        ?string $name,
        $extends,
        ?array $implements,
        ?\ast\Node $stmts,
        int $line,
        int $endLine,
        ?string $docComment
    ) : \ast\Node\Decl {
        if ($name === null) {
            $flags |= \ast\flags\CLASS_ANONYMOUS;
        }

        if ($extends !== null) {
            $ast_extends = self::_phpparser_node_to_ast_node($extends);
        } else {
            $ast_extends = null;
        }
        if ($implements !== null) {
            $ast_implements_inner = [];
            foreach ($implements as $implement) {
                $ast_implements_inner[] = self::_phpparser_node_to_ast_node($implement);
            }
            $ast_implements = astnode(\ast\AST_NAME_LIST, 0, $ast_implements_inner, $ast_implements_inner[0]->lineno);
        } else {
            $ast_implements = null;
        }

        return astdecl(
            \ast\AST_CLASS,
            $flags,
            [
                'extends'    => $ast_extends,
                'implements' => $ast_implements,
                'stmts'      => $stmts,
            ],
            $line,
            $docComment,
            $name,
            $endLine
        );
    }

    private static function _phpparser_arg_list_to_ast_arg_list(array $args, int $line) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_ARG_LIST;
        $node->flags = 0;
        $astArgs = [];
        foreach ($args as $arg) {
            $astArgs[] = self::_phpparser_node_to_ast_node($arg);
        }
        $node->lineno = $astArgs[0]->lineno ?? $line;
        $node->children = $astArgs;
        return $node;
    }

    private static function _phpparser_use_list_to_ast_use_list(?array $uses) : array {
        $astUses = [];
        foreach ($uses as $use) {
            $astUse = new \ast\Node();
            $astUse->kind = \ast\AST_USE_ELEM;
            $astUse->flags = self::_phpparser_use_type_to_ast_flags($use->type);  // FIXME
            $astUse->lineno = $use->getAttribute('startLine');
            // ast doesn't fill in an alias if it's identical to the real name,
            // but phpparser does?
            $name = implode('\\', $use->name->parts);
            $alias = $use->alias;
            $astUse->children = [
                'name' => $name,
                'alias' => $alias !== $name ? $alias : null,
            ];
            $astUses[] = $astUse;
        }
        return $astUses;
    }

    /**
     * @param int $type
     */
    private static function _phpparser_use_type_to_ast_flags($type) : int {
        switch($type) {
        case PhpParser\Node\Stmt\Use_::TYPE_NORMAL:
            return \ast\flags\USE_NORMAL;
        case PhpParser\Node\Stmt\Use_::TYPE_FUNCTION:
            return \ast\flags\USE_FUNCTION;
        case PhpParser\Node\Stmt\Use_::TYPE_CONSTANT:
            return \ast\flags\USE_CONST;
        case PhpParser\Node\Stmt\Use_::TYPE_UNKNOWN:
        default:
            return 0;
        }
    }

    private static function _ast_stmt_use($type, array $uses, int $line) : \ast\Node{
        $node = new \ast\Node();
        $node->kind = \ast\AST_USE;
        $node->flags = self::_phpparser_use_type_to_ast_flags($type);
        $node->lineno = $line;
        $node->children = $uses;
        return $node;
    }

    private static function _ast_stmt_group_use($type, $prefix, array $uses, int $line) : \ast\Node{
        $node = new \ast\Node();
        $node->kind = \ast\AST_GROUP_USE;
        $node->flags = self::_phpparser_use_type_to_ast_flags($type);
        $node->lineno = $line;
        $node->children = [
            'prefix' => $prefix,
            'uses' => self::_ast_stmt_use(0, $uses, $line),
        ];
        return $node;
    }

    private static function _ast_stmt_echo($expr, int $line) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_ECHO;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['expr' => $expr];
        return $node;
    }

    private static function _ast_stmt_return($expr, int $line) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_RETURN;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['expr' => $expr];
        return $node;
    }

    private static function _ast_if_elem($cond, $stmts, int $line) : \ast\Node {
        return astnode(\ast\AST_IF_ELEM, 0, ['cond' => $cond, 'stmts' => $stmts], $line);
    }

    private static function _phpparser_switch_list_to_ast_switch(PhpParser\Node\Stmt\Switch_ $node) {
        $stmts = [];
        $nodeLine = sl($node) ?? 0;
        foreach ($node->cases as $case) {
            $caseLine = sl($case);
            $stmts[] = astnode(
                \ast\AST_SWITCH_CASE,
                0,
                [
                    'cond' => $case->cond !== null ? self::_phpparser_node_to_ast_node($case->cond) : null,
                    'stmts' => self::_phpparser_stmtlist_to_ast_node($case->stmts, $caseLine),
                ],
                $caseLine ?? $nodeLine
            );
        }
        return astnode(\ast\AST_SWITCH, 0, [
            'cond' => self::_phpparser_node_to_ast_node($node->cond),
            'stmts' => astnode(\ast\AST_SWITCH_LIST, 0, $stmts, $stmts[0]->lineno ?? $nodeLine),
        ], $nodeLine);
    }

    private static function _phpparser_if_stmt_to_ast_if_stmt(PhpParser\Node $node) : \ast\Node {
        assert($node instanceof PhpParser\Node\Stmt\If_);
        $startLine = $node->getAttribute('startLine');
        $condLine = sl($node->cond) ?: $startLine;
        $ifElem = self::_ast_if_elem(
            self::_phpparser_node_to_ast_node($node->cond),
            self::_phpparser_stmtlist_to_ast_node($node->stmts, $condLine),
            $startLine
        );
        $ifElems = [$ifElem];
        foreach ($node->elseifs as $elseIf) {
            $ifElemLine = $elseIf->getAttribute('startLine');
            $ifElem = self::_ast_if_elem(
                self::_phpparser_node_to_ast_node($elseIf->cond),
                self::_phpparser_stmtlist_to_ast_node($elseIf->stmts, $ifElemLine),
                $ifElemLine
            );
            $ifElems[] = $ifElem;
        }
        $parserElseNode = $node->else;
        if ($parserElseNode) {
            $parserElseLine = $parserElseNode->getAttribute('startLine');
            $ifElems[] = self::_ast_if_elem(
                null,
                self::_phpparser_stmtlist_to_ast_node($parserElseNode->stmts, $parserElseLine),
                $parserElseLine
            );
        }
        return astnode(\ast\AST_IF, 0, $ifElems, $startLine);

    }

    /**
     * @suppress PhanUndeclaredProperty
     */
    private static function _ast_node_assignop(int $flags, PhpParser\Node $node, int $startLine) {
        return astnode(
            \ast\AST_ASSIGN_OP,
            $flags,
            [
                'var' => self::_phpparser_node_to_ast_node($node->var),
                'expr' => self::_phpparser_node_to_ast_node($node->expr),
            ],
            $startLine
        );
    }

    /**
     * @suppress PhanUndeclaredProperty
     */
    private static function _ast_node_binaryop(int $flags, PhpParser\Node $n, int $startLine) {
        return astnode(
            \ast\AST_BINARY_OP,
            $flags,
            self::_phpparser_nodes_to_left_right_children($n->left, $n->right),
            $startLine
        );
    }

    private static function _phpparser_nodes_to_left_right_children($left, $right) : array {
        return [
            'left' => self::_phpparser_node_to_ast_node($left),
            'right' => self::_phpparser_node_to_ast_node($right),
        ];
    }

    private static function _phpparser_propelem_to_ast_propelem(PhpParser\Node\Stmt\PropertyProperty $n, ?string $docComment) : \ast\Node{
        $children = [
            'name' => $n->name,
            'default' => $n->default ? self::_phpparser_node_to_ast_node($n->default) : null,
        ];

        $startLine = $n->getAttribute('startLine');

        return astnode(\ast\AST_PROP_ELEM, 0, $children, $startLine, self::_extract_phpdoc_comment($n->getAttribute('comments') ?? $docComment));
    }

    private static function _phpparser_constelem_to_ast_constelem(PhpParser\Node\Const_ $n, ?string $docComment) : \ast\Node{
        $children = [
            'name' => $n->name,
            'value' => self::_phpparser_node_to_ast_node($n->value),
        ];

        $startLine = $n->getAttribute('startLine');

        return astnode(\ast\AST_CONST_ELEM, 0, $children, $startLine, self::_extract_phpdoc_comment($n->getAttribute('comments') ?? $docComment));
    }

    private static function _phpparser_visibility_to_ast_visibility(int $visibility, bool $automatically_add_public = true) : int {
        $ast_visibility = 0;
        if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_PUBLIC) {
            $ast_visibility |= \ast\flags\MODIFIER_PUBLIC;
        } else if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_PROTECTED) {
            $ast_visibility |= \ast\flags\MODIFIER_PROTECTED;
        } else if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_PRIVATE) {
            $ast_visibility |= \ast\flags\MODIFIER_PRIVATE;
        } else {
            if ($automatically_add_public) {
                $ast_visibility |= \ast\flags\MODIFIER_PUBLIC;
            }
        }
        if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_STATIC) {
            $ast_visibility |= \ast\flags\MODIFIER_STATIC;
        }
        if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_ABSTRACT) {
            $ast_visibility |= \ast\flags\MODIFIER_ABSTRACT;
        }
        if ($visibility & \PHPParser\Node\Stmt\Class_::MODIFIER_FINAL) {
            $ast_visibility |= \ast\flags\MODIFIER_FINAL;
        }
        return $ast_visibility;
    }

    private static function _phpparser_property_to_ast_node(PhpParser\Node $n, int $startLine) : \ast\Node {
        assert($n instanceof \PHPParser\Node\Stmt\Property);

        $propElems = [];
        $docComment = self::_extract_phpdoc_comment($n->getAttribute('comments'));
        foreach ($n->props as $i => $prop) {
            $propElems[] = self::_phpparser_propelem_to_ast_propelem($prop, $i === 0 ? $docComment : null);
        }
        $flags = self::_phpparser_visibility_to_ast_visibility($n->flags);

        return astnode(\ast\AST_PROP_DECL, $flags, $propElems, $propElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_class_const_to_ast_node(PhpParser\Node\Stmt\ClassConst $n, int $startLine) : \ast\Node {
        $constElems = [];
        $docComment = self::_extract_phpdoc_comment($n->getAttribute('comments'));
        foreach ($n->consts as $i => $prop) {
            $constElems[] = self::_phpparser_constelem_to_ast_constelem($prop, $i === 0 ? $docComment : null);
        }
        $flags = self::_phpparser_visibility_to_ast_visibility($n->flags);

        return astnode(\ast\AST_CLASS_CONST_DECL, $flags, $constElems, $constElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_const_to_ast_node(PhpParser\Node\Stmt\Const_ $n, int $startLine) : \ast\Node {
        $constElems = [];
        $docComment = self::_extract_phpdoc_comment($n->getAttribute('comments'));
        foreach ($n->consts as $i => $prop) {
            $constElems[] = self::_phpparser_constelem_to_ast_constelem($prop, $i === 0 ? $docComment : null);
        }

        return astnode(\ast\AST_CONST_DECL, 0, $constElems, $constElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_declare_list_to_ast_declares(array $declares, int $startLine) : \ast\Node {
        $astDeclareElements = [];
        foreach ($declares as $declare) {
            $children = [
                'name' => $declare->key,
                'value' => self::_phpparser_node_to_ast_node($declare->value),
            ];
            $astDeclareElements[] = astnode(\ast\AST_CONST_ELEM, 0, $children, $declare->getAttribute('startLine'));
        }
        return astnode(\ast\AST_CONST_DECL, 0, $astDeclareElements, $startLine);

    }

    private static function _ast_stmt_declare(\ast\Node $declares, ?\ast\Node $stmts, int $startLine) : \ast\Node{
        $children = [
            'declares' => $declares,
            'stmts' => $stmts,
        ];
        return astnode(\ast\AST_DECLARE, 0, $children, $startLine);
    }

    private static function _ast_node_call($expr, $args, int $startLine) : \ast\Node{
        if (\is_string($expr)) {
            if (substr($expr, 0, 1) === '\\') {
                $expr = substr($expr, 1);
            }
            $expr = astnode(\ast\AST_NAME, \ast\flags\NAME_FQ, ['name' => $expr], $startLine);
        }
        return astnode(\ast\AST_CALL, 0, ['expr' => $expr, 'args' => $args], $startLine);
    }

    private static function _ast_node_method_call($expr, $method, \ast\Node $args, int $startLine) : \ast\Node {
        return astnode(\ast\AST_METHOD_CALL, 0, ['expr' => $expr, 'method' => $method, 'args' => $args], $startLine);
    }

    private static function _ast_node_static_call($class, $method, \ast\Node $args, int $startLine) : \ast\Node {
        // TODO: is this applicable?
        if (\is_string($class)) {
            if (substr($class, 0, 1) === '\\') {
                $expr = substr($class, 1);
            }
            $class = astnode(\ast\AST_NAME, \ast\flags\NAME_FQ, ['name' => $class], $startLine);
        }
        return astnode(\ast\AST_STATIC_CALL, 0, ['class' => $class, 'method' => $method, 'args' => $args], $startLine);
    }

    private static function _extract_phpdoc_comment($comments) : ?string {
        if (\is_string($comments)) {
            return $comments;
        }
        if ($comments === null) {
            return null;
        }
        assert(\is_array($comments));
        if (\count($comments) === 0) {
            return null;
        }
        for ($i = \count($comments) - 1; $i >= 0; $i--) {
            if ($comments[$i] instanceof PhpParser\Comment\Doc) {
                return $comments[$i]->getText();
            } else {
                // e.g. PhpParser\Comment; for a line comment
            }
        }
        return null;
        // return var_export($comments, true);
    }

    private static function _phpparser_list_to_ast_list(PhpParser\Node\Expression\List_ $n, int $startLine) : \ast\Node {
        $astItems = [];
        foreach ($n->items as $item) {
            if ($item === null) {
                $astItems[] = null;
            } else {
                $astItems[] = astnode(\ast\AST_ARRAY_ELEM, 0, [
                    'value' => self::_phpparser_node_to_ast_node($item->value),
                    'key' => $item->key !== null ? self::_phpparser_node_to_ast_node($item->key) : null,
                ], $item->getAttribute('startLine'));
            }
        }
        return astnode(\ast\AST_ARRAY, \ast\flags\ARRAY_SYNTAX_LIST, $astItems, $startLine);
    }

    private static function _phpparser_array_to_ast_array(PhpParser\Node\Expression\ArrayCreationExpression $n, int $startLine) : \ast\Node {
        $astItems = [];
        foreach ($n->items as $item) {
            if ($item === null) {
                $astItems[] = null;
            } else {
                $astItems[] = astnode(\ast\AST_ARRAY_ELEM, 0, [
                    'value' => self::_phpparser_node_to_ast_node($item->value),
                    'key' => $item->key !== null ? self::_phpparser_node_to_ast_node($item->key) : null,
                ], $item->getAttribute('startLine'));
            }
        }
        return astnode(\ast\AST_ARRAY, \ast\flags\ARRAY_SYNTAX_SHORT, $astItems, $startLine);
    }

    private static function _phpparser_propertyfetch_to_ast_prop(PhpParser\Node\Expression\PropertyFetch $n, int $startLine) : ?\ast\Node {
        $name = $n->name;
        if (is_object($name)) {
            $name = self::_phpparser_node_to_ast_node($name);
        }
        if ($name === null) {
            if (self::$should_add_placeholders) {
                $name = '__INCOMPLETE_PROPERTY__';
            } else {
                return null;
            }
        }
        return astnode(\ast\AST_PROP, 0, [
            'expr'  => self::_phpparser_node_to_ast_node($n->var),
            'prop'  => is_object($name) ?  : $name,
        ], $startLine);
    }

    private static function _phpparser_classconstfetch_to_ast_classconstfetch(PhpParser\Node\Expression\ClassConstFetch $n, int $startLine) : ?\ast\Node {
        $name = $n->name;
        if (is_object($name)) {
            $name = self::_phpparser_node_to_ast_node($name);
        }
        if ($name === null) {
            if (self::$should_add_placeholders) {
                $name = '__INCOMPLETE_CLASS_CONST__';
            } else {
                return null;
            }
        }
        return astnode(\ast\AST_CLASS_CONST, 0, [
            'class' => self::_phpparser_node_to_ast_node($n->class),
            'const' => $name,
        ], $startLine);
    }

    /**
     * @suppress PhanDeprecatedProperty TODO: figure out alternative
     */
    private static function _phpparser_name_to_string(PhpParser\Node\Name $name) : string {
        return implode('\\', $name->parts);
    }
}

/**
 * @suppress PhanTypeMismatchProperty https://github.com/etsy/phan/issues/609
 * @suppress PhanUndeclaredProperty - docComment really exists.
 * NOTE: this may be removed in the future.
 *
 * Phan was used while developing this. The asserts can be cleaned up in the future.
 */
function astnode(int $kind, int $flags, ?array $children, int $lineno, ?string $docComment = null) : \ast\Node {
    $node = new \ast\Node();
    $node->kind = $kind;
    $node->flags = $flags;
    $node->lineno = $lineno;
    $node->children = $children;
    if (\is_string($docComment)) {
        $node->docComment = $docComment;
    }
    return $node;
}

/**
 * @suppress PhanTypeMismatchProperty https://github.com/etsy/phan/issues/609
 * @suppress PhanUndeclaredProperty - docComment really exists.
 * NOTE: this may be removed in the future.
 *
 * Phan was used while developing this. The asserts can be cleaned up in the future.
 */
function astdecl(int $kind, int $flags, ?array $children, int $lineno, string $docComment = null, string $name = null, int $endLineno = 0) : \ast\Node\Decl {
    $node = new \ast\Node\Decl();
    $node->kind = $kind;
    $node->flags = $flags;
    $node->lineno = $lineno;
    $node->children = $children;
    if (\is_string($docComment)) {
        $node->docComment = $docComment;
    }
    $node->name = $name;
    $node->endLineno = $endLineno;
    return $node;
}

function sl($node) : ?int {
    if ($node instanceof PhpParser\Node) {
        return $node->getAttribute('startLine');
    }
    return null;
}

function el($node) : ?int {
    if ($node instanceof PhpParser\Node) {
        return $node->getAttribute('endLine');
    }
    return null;
}
