<?php declare(strict_types=1);
namespace TolerantASTConverter;

use Microsoft\PhpParser;
use Microsoft\PhpParser\DiagnosticsProvider;
use Microsoft\PhpParser\Parser;
use Microsoft\PhpParser\TokenKind;

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
                $childNodeLine = self::getEndLine($parserNode);
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
            $childNodeLine = self::getEndLine($parserNode);
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
     * @param PhpParser\Node|PhpParser\Token $n - The node from PHP-Parser
     * @return \ast\Node|\ast\Node[]|string|int|float|bool|null - whatever \ast\parse_code would return as the equivalent.
     * @suppress PhanUndeclaredProperty
     */
    private static final function _phpparser_node_to_ast_node($n) {
        if (!($n instanceof PhpParser\Node) && !($n instanceof PhpParser\Token)) {
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

    private static function getEndLine(PhpParser\Node $n) : int {
        // TODO: binary search in an array mapping line number to character offset?
        // Currently returns character offset.
        return $n->getEndPosition();
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
            'Microsoft\PhpParser\Node\Expression\SubscriptExpression'            => function(PhpParser\Node\Expression\SubscriptExpression $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_DIM, 0, [
                    'expr' => self::_phpparser_node_to_ast_node($n->postfixExpression),
                    'dim' => $n->accessExpression !== null ? self::_phpparser_node_to_ast_node($n->accessExpression) : null,
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AssignmentExpression'                    => function(PhpParser\Node\Expression\AssignmentExpression $n, int $startLine) : ?\ast\Node {
                $opKind = $n->operator->kind;
                assert($opKind === TokenKind::EqualsToken);
                // FIXME switch on $n->kind
                return self::_ast_node_assign(
                    self::_phpparser_node_to_ast_node($n->leftOperand),
                    self::_phpparser_node_to_ast_node($n->rightOperand),
                    $startLine,
                    $n->byRef !== null
                );
            },
            'Microsoft\PhpParser\Node\Expression\BinaryExpression' => function(PhpParser\Node\Expression\BinaryExpression $n, int $startLine) : \ast\Node {
                static $lookup = [
                    TokenKind::AmpersandToken               => \ast\flags\BINARY_BITWISE_AND,
                    TokenKind::BarToken                     => \ast\flags\BINARY_BITWISE_OR,
                    TokenKind::CaretToken                   => \ast\flags\BINARY_BITWISE_XOR,
                    TokenKind::AmpersandAmpersandToken      => \ast\flags\BINARY_BOOL_AND,
                    TokenKind::BarBarToken                  => \ast\flags\BINARY_BOOL_OR,
                    TokenKind::QuestionQuestionToken        => \ast\flags\BINARY_COALESCE,
                ];
                $kind = $n->operator->kind;
                if ($kind === TokenKind::InstanceOfKeyword) {
                    return astnode(\ast\AST_INSTANCEOF, 0, [
                        'expr'  => self::_phpparser_node_to_ast_node($n->leftOperand),
                        'class' => self::_phpparser_node_to_ast_node($n->rightOperand),
                    ], $startLine);
                }
                $astKind = $lookup[$kind] ?? null;
                assert($astKind !== null, "missing $kind");
                return self::_ast_node_binaryop($astKind, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\UnaryOpExpression' => function(PhpParser\Node\Expression\UnaryOpExpression $n, int $startLine) : \ast\Node {
                static $lookup = [
                    TokenKind::TildeToken                   => \ast\flags\UNARY_BITWISE_NOT,
                    TokenKind::MinusToken                   => \ast\flags\UNARY_MINUS,
                    TokenKind::PlusToken                    => \ast\flags\UNARY_PLUS,
                ];
                $kind = $n->operator->kind;
                $astKind = $lookup[$kind] ?? null;
                assert($astKind !== null, "missing $kind");
                return self::_ast_node_unary_op($astKind, self::_phpparser_node_to_ast_node($n->operand), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\CastExpression' => function(PhpParser\Node\Expression\CastExpression $n, int $startLine) : \ast\Node {
                static $lookup = [
                    TokenKind::ArrayCastToken   => \ast\flags\TYPE_ARRAY,
                    TokenKind::BoolCastToken    => \ast\flags\TYPE_BOOL,
                    TokenKind::DoubleCastToken  => \ast\flags\TYPE_DOUBLE,
                    TokenKind::IntCastToken     => \ast\flags\TYPE_LONG,
                    TokenKind::ObjectCastToken  => \ast\flags\TYPE_OBJECT,
                    TokenKind::StringCastToken  => \ast\flags\TYPE_STRING,
                    TokenKind::DoubleCastToken  => \ast\flags\TYPE_DOUBLE,
                    TokenKind::UnsetCastToken   => \ast\flags\TYPE_VOID,
                ];
                $kind = $n->castType->kind;
                $astKind = $lookup[$kind] ?? null;
                assert($astKind !== null, "missing $kind");
                return self::_ast_node_cast($astKind, $n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\AnonymousFunctionCreationExpression' => function(PhpParser\Node\Expression\AnonymousFunctionCreationExpression $n, int $startLine) : \ast\Node {
                // TODO: is there a corresponding flag for $n->static? $n->byRef?
                return self::_ast_decl_closure(
                    $n->byRefToken !== null,
                    $n->staticModifier !== null,
                    self::_phpparser_params_to_ast_params($n->parameters, $startLine),
                    self::_phpparser_closure_uses_to_ast_closure_uses($n->anonymousFunctionUseClause->useVariableNameList, $startLine),
                    self::_phpparser_stmtlist_to_ast_node($n->compoundStatementOrSemicolon->statements, $startLine),
                    self::_phpparser_type_to_ast_node($n->returnType, self::getEndLine($n->returnType) ?: $startLine),
                    $startLine,
                    self::getEndLine($n),
                    $n->getDocCommentText()
                );
                // FIXME: add a test of ClassQualifiedName to php-ast
            },
            'Microsoft\PhpParser\Node\Expression\ScopedPropertyAccessExpression' => function(PhpParser\Node\Expression\ScopedPropertyAccessExpression $n, int $startLine) : ?\ast\Node {
                $memberName = $n->memberName;
                if ($memberName instanceof PhpParser\Node\Expression\Variable) {
                    return astnode(
                        \ast\AST_STATIC_PROP,
                        0,
                        [
                            'class' => self::_phpparser_node_to_ast_node($n->scopeResolutionQualifier),
                            'prop' => self::_phpparser_node_to_ast_node($n->memberName),
                        ],
                        $startLine
                    );
                } else {
                    \assert($memberName instanceof PhpParser\Token);
                    return self::_phpparser_classconstfetch_to_ast_classconstfetch($n->scopeResolutionQualifier, $memberName, $startLine);
                }
            },
            'Microsoft\PhpParser\Node\Expression\CloneExpression' => function(PhpParser\Node\Expression\CloneExpression $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_CLONE, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expression)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ErrorControlExpression' => function(PhpParser\Node\Expression\ErrorControlExpression $n, int $startLine) : \ast\Node {
                return self::_ast_node_unary_op(\ast\flags\UNARY_SILENCE, self::_phpparser_node_to_ast_node($n->operand), $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\EmptyIntrinsicExpression' => function(PhpParser\Node\Expression\EmptyIntrinsicExpression $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_EMPTY, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expression)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\EvalIntrinsicExpression' => function(PhpParser\Node\Expression\EvalIntrinsicExpression $n, int $startLine) : \ast\Node {
                return self::_ast_node_eval(
                    self::_phpparser_node_to_ast_node($n->expression),
                    $startLine
                );
            },
            'Microsoft\PhpParser\MissingToken' => function(PhpParser\MissingToken $n, int $startLine) {
                // This is where PhpParser couldn't parse a node.
                // TODO: handle this.
                return null;
            },
            'Microsoft\PhpParser\Node\Expression\ExitIntrinsicExpression' => function(PhpParser\Node\Expression\ExitIntrinsicExpression $n, int $startLine) {
                return astnode(\ast\AST_EXIT, 0, ['expr' => self::_phpparser_node_to_ast_node($n->expression)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\CallExpression' => function(PhpParser\Node\Expression\CallExpression $n, int $startLine) : \ast\Node {
                $callableExpression = $n->callableExpression;
                $argList = self::_phpparser_arg_list_to_ast_arg_list($n->argumentExpressionList, $startLine);
                if ($callableExpression instanceof PhpParser\Node\Expression\MemberAccessExpression) {  // $a->f()
                    return self::_ast_node_method_call(
                        self::_phpparser_node_to_ast_node($callableExpression->dereferencableExpression),
                        self::_phpparser_node_to_ast_node($callableExpression->memberName),
                        $argList,
                        $startLine
                    );
                } else if ($callableExpression instanceof PhpParser\Node\Expression\ScopedPropertyAccessExpression) {  // a::f()
                    return self::_ast_node_static_call(
                        self::_phpparser_node_to_ast_node($callableExpression->scopeResolutionQualifier),
                        self::_phpparser_node_to_ast_node($callableExpression->memberName),
                        $argList,
                        $startLine
                    );
                } else {  // f()
                    return self::_ast_node_call(
                        self::_phpparser_node_to_ast_node($callableExpression),
                        $argList,
                        $startLine
                    );
                }
            },
            'Microsoft\PhpParser\Node\Expression\ScriptInclusionExpression' => function(PhpParser\Node\Expression\ScriptInclusionExpression $n, int $startLine) : \ast\Node {
                return self::_ast_node_include(
                    self::_phpparser_node_to_ast_node($n->expression),
                    $startLine,
                    $n->requireOrIncludeKeyword
                );
            },
            'Microsoft\PhpParser\Node\Expression\IssetIntrinsicExpression' => function(PhpParser\Node\Expression\IssetIntrinsicExpression $n, int $startLine) : \ast\Node {
                $astIssets = [];
                foreach ($n->expressions->children as $var) {
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
            'Microsoft\PhpParser\Node\Expression\ArrayCreationExpression' => function(PhpParser\Node\Expression\ArrayCreationExpression $n, int $startLine) : \ast\Node {
                return self::_phpparser_list_to_ast_list($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\ObjectCreationExpression' => function(PhpParser\Node\Expression\ObjectCreationExpression $n, int $startLine) : \ast\Node {
                return astnode(\ast\AST_NEW, 0, [
                    'class' => self::_phpparser_node_to_ast_node($n->classTypeDesignator),
                    'args' => self::_phpparser_arg_list_to_ast_arg_list($n->argumentExpressionList, $startLine),
                ], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PrefixUpdateExpression' => function(PhpParser\Node\Expression\PrefixUpdateExpression $n, int $startLine) : \ast\Node {
                $name = self::_token_to_string($n->incrementOrDecrementOperator);
                switch ($name) {
                case '+': $type = \ast\AST_PRE_INC; break;
                case '-': $type = \ast\AST_PRE_DEC; break;
                default: throw new \RuntimeException('impossible operator ' . $type);
                }

                return astnode($type, 0, ['var' => self::_phpparser_node_to_ast_node($n->operand)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PostfixUpdateExpression' => function(PhpParser\Node\Expression\PostfixUpdateExpression $n, int $startLine) : \ast\Node {
                $name = self::_token_to_string($n->incrementOrDecrementOperator);
                switch ($name) {
                case '+': $type = \ast\AST_POST_INC; break;
                case '-': $type = \ast\AST_POST_DEC; break;
                default: throw new \RuntimeException('impossible operator ' . $type);
                }

                return astnode($type, 0, ['var' => self::_phpparser_node_to_ast_node($n->operand)], $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\PrintIntrinsicExpression' => function(PhpParser\Node\Expression\PrintIntrinsicExpression $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_PRINT,
                    0,
                    ['expr' => self::_phpparser_node_to_ast_node($n->expression)],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\MemberAccessExpression' => function(PhpParser\Node\Expression\MemberAccessExpression $n, int $startLine) : ?\ast\Node {
                return self::_phpparser_propertyfetch_to_ast_prop($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\TernaryExpression' => function(PhpParser\Node\Expression\TernaryExpression $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_CONDITIONAL,
                    0,
                    [
                        'cond' => self::_phpparser_node_to_ast_node($n->condition),
                        'true' => $n->ifExpression !== null ? self::_phpparser_node_to_ast_node($n->ifExpression) : null,
                        'false' => self::_phpparser_node_to_ast_node($n->elseExpression),
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Expression\Variable' => function(PhpParser\Node\Expression\Variable $n, int $startLine) : ?\ast\Node {
                return self::_ast_node_variable($n->name, $startLine);
            },
            'Microsoft\PhpParser\Node\Expression\YieldExpression' => function(PhpParser\Node\Expression\YieldExpression $n, int $startLine) : \ast\Node {
                $str = strtolower(self::_token_to_string($n->yieldOrYieldFromKeyword));
                switch ($str) {
                case 'yield': $kind = \ast\AST_YIELD;
                default:
                if (!preg_match('@^yield\s+from$@', $str)) {
                    throw new \RuntimeException('Invalid yield expression');
                }
                $kind = \ast\AST_YIELD_FROM;
                }
                $arrayElement = $n->arrayElement;
                return astnode(
                    $kind,
                    0,
                    [
                        'value' => $arrayElement->elementValue !== null ? self::_phpparser_node_to_ast_node($arrayElement->elementValue) : null,
                        'key'   => $arrayElement->elementKey   !== null ? self::_phpparser_node_to_ast_node($arrayElement->elementKey) : null,
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Parameter' => function(PhpParser\Node\Parameter $n, int $startLine) : \ast\Node {
                $typeLine = self::getEndLine($n->typeDeclaration) ?: $startLine;
                $defaultLine = self::getEndLine($n->typeDeclaration) ?: $typeLine;
                return self::_ast_node_param(
                    $n->questionToken !== null,
                    $n->byRefToken !== null,
                    $n->dotDotDotToken !== null,
                    self::_phpparser_type_to_ast_node($n->typeDeclaration, $typeLine),
                    $n->variableName,
                    self::_phpparser_type_to_ast_node($n->default, $defaultLine),
                    $startLine
                );
            },
            /*
            // FIXME parse
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
             */
            /** @return int|float */
            'Microsoft\PhpParser\Node\NumericLiteral' => function(PhpParser\Node\NumericLiteral $n, int $startLine) {
                $text = self::_token_to_string($n->children);
                $asInt = filter_var($text, FILTER_VALIDATE_INT);
                if ($asInt !== false) {
                    return $asInt;
                }
                return (float)$text;
            },
            'Microsoft\PhpParser\Node\StringLiteral' => function(PhpParser\Node\StringLiteral $n, int $startLine) : string {
                // FIXME utilities to convert StringLiteral to string
                // FIXME build AST_ENCAPS_LIST
                $text = implode('', array_map(function($x) { return self::_token_to_string($x); }, $n->children));
                // TODO: Handle shell_exec
                // TODO: Need a way to escape backslashes

                return $text;
            },
            /* FIXME QualifiedName
            'Microsoft\PhpParser\Node\Scalar\MagicConst\ClassDeclaration' => function(PhpParser\Node\Scalar\MagicConst\ClassDeclaration $n, int $startLine) : \ast\Node {
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
            }, */
            'Microsoft\PhpParser\Node\Statement\BreakOrContinueStatement' => function(PhpParser\Node\Statement\BreakOrContinueStatement $n, int $startLine) : \ast\Node {
                switch ($n->breakOrContinueKeyword->length) {
                case 5: $kind = \ast\AST_BREAK;
                case 8: $kind = \ast\AST_CONTINUE;
                default: throw new \RuntimeException("Invalid BreakOrContinueStatement token");
                }
                return astnode($kind, 0, ['depth' => isset($n->breakoutLevel) ? (int)self::_token_to_string($n->breakoutLevel) : null], $startLine);
            },
            /*
            'Microsoft\PhpParser\Node\CatchClause' => function(PhpParser\Node\CatchClause $n, int $startLine) : \ast\Node {
                // TODO: Change after https://github.com/Microsoft/tolerant-php-parser/issues/103 is supported
                return self::_ast_stmt_catch(
                    self::_phpparser_name_list_to_ast_name_list($n->qualifiedName, $startLine),
                    $n->var,
                    self::_phpparser_stmtlist_to_ast_node($n->compoundstatement->statements, $startLine),
                    $startLine
                );
            },
             */
            /*
            'Microsoft\PhpParser\Node\Statement\ClassDeclaration' => function(PhpParser\Node\Statement\ClassDeclaration $n, int $startLine) : \ast\Node {
                $endLine = self::getEndLine($n) ?: $startLine;
                return self::_ast_stmt_class(
                    self::_phpparser_class_modifier_to_ast_class_flags($n->abstractOrFinalModifier),
                    self::_token_to_string($n->name),
                    $n->classBaseClause !== null? self::_phpparser_name_to_string($n->classBaseClause->baseClass) : null,
                    $n->classInterfaceClause,
                    self::_phpparser_stmtlist_to_ast_node($n->classMembers, $startLine),
                    $startLine,
                    $endLine,
                    $n->getDocCommentText()
                );
            },
             */
            'Microsoft\PhpParser\Node\ClassConstDeclaration' => function(PhpParser\Node\ClassConstDeclaration $n, int $startLine) : \ast\Node {
                return self::_phpparser_class_const_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\MethodDeclaration' => function(PhpParser\Node\MethodDeclaration $n, int $startLine) : \ast\Node {
                return astdecl(
                    \ast\AST_METHOD,
                    self::_phpparser_visibility_to_ast_visibility($n->modifiers) | ($n->byRefToken !== null ? \ast\flags\RETURNS_REF : 0),
                    [
                        'params' => self::_phpparser_params_to_ast_params($n->parameters, $startLine),
                        'uses' => null,  // TODO: anonymous class?
                        'stmts' => is_array($n->compoundStatementOrSemicolon->statements) ? self::_phpparser_stmtlist_to_ast_node($n->compoundStatementOrSemicolon->statements, $startLine) : null,
                        'returnType' => self::_phpparser_type_to_ast_node($n->returnType, self::getEndLine($n->returnType) ?: $startLine)
                    ],
                    $startLine,
                    $n->getDocCommentText(),
                    $n->name,
                    self::getEndLine($n)
                );
            },
            'Microsoft\PhpParser\Node\Statement\ConstDeclaration' => function(PhpParser\Node\Statement\ConstDeclaration $n, int $startLine) : \ast\Node {
                return self::_phpparser_const_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Statement\DeclareStatement' => function(PhpParser\Node\Statement\DeclareStatement $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_declare(
                    self::_phpparser_declare_list_to_ast_declares($n->declareDirective, $startLine),
                    $n->statements !== null ? self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine) : null,
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\Do_' => function(PhpParser\Node\Statement\Do_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_do_while(
                    self::_phpparser_node_to_ast_node($n->cond),
                    self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine),
                    $startLine
                );
            },
            /**
             * @return \ast\Node|\ast\Node[]
             */
            'Microsoft\PhpParser\Node\Statement\Echo_' => function(PhpParser\Node\Statement\Echo_ $n, int $startLine) {
                $astEchos = [];
                foreach ($n->exprs as $expr) {
                    $astEchos[] = self::_ast_stmt_echo(
                        self::_phpparser_node_to_ast_node($expr),
                        $startLine
                    );
                }
                return count($astEchos) === 1 ? $astEchos[0] : $astEchos;
            },
            'Microsoft\PhpParser\Node\Statement\Foreach_' => function(PhpParser\Node\Statement\Foreach_ $n, int $startLine) : \ast\Node {
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
                        'expr' => self::_phpparser_node_to_ast_node($n->expression),
                        'value' => $value,
                        'key' => $n->keyVar !== null ? self::_phpparser_node_to_ast_node($n->keyVar) : null,
                        'stmts' => self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine),
                    ],
                    $startLine
                );
                //return self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine);
            },
            'Microsoft\PhpParser\Node\Statement\Finally_' => function(PhpParser\Node\Statement\Finally_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine);
            },
            'Microsoft\PhpParser\Node\Statement\Function_' => function(PhpParser\Node\Statement\Function_ $n, int $startLine) : \ast\Node {
                $endLine = self::getEndLine($n) ?: $startLine;
                $returnType = $n->returnType;
                $returnTypeLine = self::getEndLine($returnType) ?: $endLine;
                $astReturnType = self::_phpparser_type_to_ast_node($returnType, $returnTypeLine);

                return self::_ast_decl_function(
                    $n->byRef,
                    $n->name,
                    self::_phpparser_params_to_ast_params($n->parameters, $startLine),
                    null,  // uses
                    self::_phpparser_type_to_ast_node($returnType, $returnTypeLine),
                    self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine),
                    $startLine,
                    $endLine,
                    $n->getDocCommentText()
                );
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\Global_' => function(PhpParser\Node\Statement\Global_ $n, int $startLine) {
                $globalNodes = [];
                foreach ($n->vars as $var) {
                    $globalNodes[] = astnode(\ast\AST_GLOBAL, 0, ['var' => self::_phpparser_node_to_ast_node($var)], self::getEndLine($var) ?: $startLine);
                }
                return \count($globalNodes) === 1 ? $globalNodes[0] : $globalNodes;
            },
            'Microsoft\PhpParser\Node\Statement\If_' => function(PhpParser\Node\Statement\If_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_if_stmt_to_ast_if_stmt($n);
            },
            'Microsoft\PhpParser\Node\Statement\InlineHTML' => function(PhpParser\Node\Statement\InlineHTML $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_echo($n->value, $startLine);
            },
            'Microsoft\PhpParser\Node\Statement\Interface_' => function(PhpParser\Node\Statement\Interface_ $n, int $startLine) : \ast\Node {
                $endLine = self::getEndLine($n) ?: $startLine;
                return self::_ast_stmt_class(
                    \ast\flags\CLASS_INTERFACE,
                    $n->name,
                    null,
                    null,
                    is_array($n->statements) ? self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine) : null,
                    $startLine,
                    $endLine,
                    $n->getDocCommentText()
                );
            },
            'Microsoft\PhpParser\Node\Statement\For_' => function(PhpParser\Node\Statement\For_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_FOR,
                    0,
                    [
                        'init' => \count($n->init) > 0 ? self::_phpparser_expr_list_to_expr_list($n->init, $startLine) : null,
                        'cond' => \count($n->cond) > 0 ? self::_phpparser_expr_list_to_expr_list($n->cond, $startLine) : null,
                        'loop' => \count($n->loop) > 0 ? self::_phpparser_expr_list_to_expr_list($n->loop, $startLine) : null,
                        'stmts' => self::_phpparser_stmtlist_to_ast_node($n->statements ?? [], $startLine),
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\GroupUse' => function(PhpParser\Node\Statement\GroupUse $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_group_use(
                    $n->type,
                    self::_phpparser_name_to_string($n->prefix),
                    self::_phpparser_use_list_to_ast_use_list($n->uses),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\Namespace_' => function(PhpParser\Node\Statement\Namespace_ $n, int $startLine) : \ast\Node {
                $nodeDumper = new PhpParser\NodeDumper([
                    'dumpComments' => true,
                    'dumpPositions' => true,
                ]);
                return astnode(
                    \ast\AST_NAMESPACE,
                    0,
                    [
                        'name' => $n->name !== null ? self::_phpparser_name_to_string($n->name) : null,
                        'stmts' => isset($n->statements) ? self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine) : null,
                    ],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\Nop' => function(PhpParser\Node\Statement\Nop $n, int $startLine) : array {
                // `;;`
                return [];
            },
            'Microsoft\PhpParser\Node\Statement\Property' => function(PhpParser\Node\Statement\Property $n, int $startLine) : \ast\Node {
                return self::_phpparser_property_to_ast_node($n, $startLine);
            },
            'Microsoft\PhpParser\Node\Statement\Return_' => function(PhpParser\Node\Statement\Return_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_return($n->expression !== null ? self::_phpparser_node_to_ast_node($n->expression) : null, $startLine);
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\Static_' => function(PhpParser\Node\Statement\Static_ $n, int $startLine) {
                $staticNodes = [];
                foreach ($n->vars as $var) {
                    $staticNodes[] = astnode(\ast\AST_STATIC, 0, [
                        'var' => astnode(\ast\AST_VAR, 0, ['name' => $var->name], self::getEndLine($var) ?: $startLine),
                        'default' => $var->default !== null ? self::_phpparser_node_to_ast_node($var->default) : null,
                    ], self::getEndLine($var) ?: $startLine);
                }
                return \count($staticNodes) === 1 ? $staticNodes[0] : $staticNodes;
            },
            'Microsoft\PhpParser\Node\Statement\Switch_' => function(PhpParser\Node\Statement\Switch_ $n, int $startLine) : \ast\Node {
                return self::_phpparser_switch_list_to_ast_switch($n);
            },
            'Microsoft\PhpParser\Node\Statement\Throw_' => function(PhpParser\Node\Statement\Throw_ $n, int $startLine) : \ast\Node {
                return astnode(
                    \ast\AST_THROW,
                    0,
                    ['expr' => self::_phpparser_node_to_ast_node($n->expression)],
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\Trait_' => function(PhpParser\Node\Statement\Trait_ $n, int $startLine) : \ast\Node {
                $endLine = self::getEndLine($n) ?: $startLine;
                return self::_ast_stmt_class(
                    \ast\flags\CLASS_TRAIT,
                    $n->name,
                    null,
                    null,
                    self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine),
                    $startLine,
                    $endLine,
                    $n->getDocCommentText()
                );
            },
            'Microsoft\PhpParser\Node\Statement\TraitUse' => function(PhpParser\Node\Statement\TraitUse $n, int $startLine) : \ast\Node {
                if (\is_array($n->adaptations) && \count($n->adaptations) > 0) {
                    $adaptations_inner = array_map(function(PhpParser\Node\Statement\TraitUseAdaptation $n) : \ast\Node {
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
            'Microsoft\PhpParser\Node\Statement\TraitUseAdaptation\Alias' => function(PhpParser\Node\Statement\TraitUseAdaptation\Alias $n, int $startLine) : \ast\Node {
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
            'Microsoft\PhpParser\Node\Statement\TraitUseAdaptation\Precedence' => function(PhpParser\Node\Statement\TraitUseAdaptation\Precedence $n, int $startLine) : \ast\Node {
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
            'Microsoft\PhpParser\Node\Statement\TryCatch' => function(PhpParser\Node\Statement\TryCatch $n, int $startLine) : \ast\Node {
                if (!is_array($n->catches)) {
                    throw new \Error(sprintf("Unsupported type %s\n%s", get_class($n), var_export($n->catches, true)));
                }
                return self::_ast_node_try(
                    self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine), // $n->try
                    self::_phpparser_catchlist_to_ast_catchlist($n->catches, $startLine),
                    isset($n->finally) ? self::_phpparser_stmtlist_to_ast_node($n->finally->statements, self::getEndLine($n->finally)) : null,
                    $startLine
                );
            },
            /** @return \ast\Node|\ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\Unset_' => function(PhpParser\Node\Statement\Unset_ $n, int $startLine) {
                $stmts = [];
                foreach ($n->vars as $var) {
                    $stmts[] = astnode(\ast\AST_UNSET, 0, ['var' => self::_phpparser_node_to_ast_node($var)], self::getEndLine($var) ?: $startLine);
                }
                return \count($stmts) === 1 ? $stmts[0] : $stmts;
            },
            'Microsoft\PhpParser\Node\Statement\Use_' => function(PhpParser\Node\Statement\Use_ $n, int $startLine) : \ast\Node {
                return self::_ast_stmt_use(
                    $n->type,
                    self::_phpparser_use_list_to_ast_use_list($n->uses),
                    $startLine
                );
            },
            'Microsoft\PhpParser\Node\Statement\While_' => function(PhpParser\Node\Statement\While_ $n, int $startLine) : \ast\Node {
                return self::_ast_node_while(
                    self::_phpparser_node_to_ast_node($n->cond),
                    self::_phpparser_stmtlist_to_ast_node($n->statements, $startLine),
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

    private static function _ast_node_cast(int $flags, PhpParser\Node\Expression\CastExpression $n, int $line) : \ast\Node {
        return astnode(\ast\AST_CAST, $flags, ['expr' => self::_phpparser_node_to_ast_node($n->expression)], self::getEndLine($n) ?: $line);
    }

    private static function _ast_node_eval($expr, int $line) : \ast\Node {
        return astnode(\ast\AST_INCLUDE_OR_EVAL, \ast\flags\EXEC_EVAL, ['expr' => $expr], $line);
    }

    private static function _phpparser_include_token_to_ast_include_flags(PhpParser\Token $type) : int {
        $typeName = strtolower(self::_token_to_string($type));
        switch($typeName) {
        case 'include':
            return \ast\flags\EXEC_INCLUDE;
        case 'include_once':
            return \ast\flags\EXEC_INCLUDE_ONCE;
        case 'require':
            return \ast\flags\EXEC_REQUIRE;
        case 'require_once':
            return \ast\flags\EXEC_REQUIRE_ONCE;
        default:
            throw new \Error("Unrecognized PhpParser include/require type $typeName");
        }
    }
    private static function _ast_node_include($expr, int $line, PhpParser\Token $type) : \ast\Node {
        $flags = self::_phpparser_include_token_to_ast_include_flags($type);
        return astnode(\ast\AST_INCLUDE_OR_EVAL, $flags, ['expr' => $expr], $line);
    }

    /**
     * @param PhpParser\Node|null $type
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
    private static function _ast_node_param(bool $isNullable, bool $byRef, bool $variadic, $type, $name, $default, int $line) : \ast\Node {
        $node = new \ast\Node;
        $node->kind = \ast\AST_PARAM;
        $node->flags = ($byRef ? \ast\flags\PARAM_REF : 0) | ($variadic ? \ast\flags\PARAM_VARIADIC : 0);
        $node->lineno = $line;
        $node->children = [
            'type' => $type,
            'name' => $name,
            'default' => $default,
        ];
        if ($isNullable) {
            return self::_ast_node_nullable_type(
                $node,
                $startLine
            );
        }

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

    private static function _ast_node_name(PhpParser\Node\QualifiedName $name, int $line) : \ast\Node {
        // FIXME: skip over whitespace and \\
        $implodedParts = self::_phpparser_name_to_string($name);
        if ($n->globalSpecifier !== null) {
            return astnode(\ast\AST_NAME, \ast\flags\NAME_FQ, ['name' => $implodedParts], $line);
        }
        return astnode(\ast\AST_NAME, \ast\flags\NAME_NOT_FQ, ['name' => $implodedParts], $line);
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

    private static function _phpparser_params_to_ast_params(?PhpParser\Node\DelimitedList\ParameterDeclarationList $parserParams, int $line) : \ast\Node {
        $newParams = [];
        foreach ($parserParams->children as $parserNode) {
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
        $node->lineno = self::getStartLine($parserNode);
        $node->children = null;
        return $node;
    }

    /**
     * @param PhpParser\Node\DelimitedList\UseVariableNameList $uses
     * @param int $line
     * @return ?\ast\Node
     */
    private static function _phpparser_closure_uses_to_ast_closure_uses(
        PhpParser\Node\DelimitedList\UseVariableNameList $uses,
        int $line
    ) {
        if (count($uses) === 0) {
            return null;
        }
        $astUses = [];
        foreach ($uses as $use) {
            $astUses[] = astnode(\ast\AST_CLOSURE_VAR, $use->byRef ? 1 : 0, ['name' => $use->var], self::getStartLine($use));
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

    private static function _phpparser_class_modifier_to_ast_class_flags(?PhpParser\Token $flags) : int {
        if ($flags === null) {
            return 0;
        }
        $name = strtolower(self::_token_to_string($flags));
        $astFlags = 0;
        if ($name === 'abstract') {
            $astFlags |= \ast\flags\CLASS_ABSTRACT;
        }
        if ($flags === 'final') {
            $astFlags |= \ast\flags\CLASS_FINAL;
        }
        return $astFlags;
    }

    /**
     * @param int $flags
     * @param ?string $name
     * @param ?string $extends TODO
     * @param ?array $implements
     * @param ?\ast\Node $stmts
     * @param int $line
     * @param int $endLine
     * @suppress PhanTypeMismatchProperty (?string to string|null is incorrectly reported)
     */
    private static function _ast_stmt_class(
        int $flags,
        ?string $name,
        ?string $extends,
        ?PhpParser\Node\ClassInterfaceClause $implements,
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

    private static function _phpparser_arg_list_to_ast_arg_list(?PhpParser\Node\DelimitedList\ArgumentExpressionList $args, int $line) : \ast\Node {
        $node = new \ast\Node();
        $node->kind = \ast\AST_ARG_LIST;
        $node->flags = 0;
        $astArgs = [];
        foreach ($args->children ?? [] as $arg) {
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
            $astUse->lineno = self::getStartLine($use);
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
        case PhpParser\Node\Statement\Use_::TYPE_NORMAL:
            return \ast\flags\USE_NORMAL;
        case PhpParser\Node\Statement\Use_::TYPE_FUNCTION:
            return \ast\flags\USE_FUNCTION;
        case PhpParser\Node\Statement\Use_::TYPE_CONSTANT:
            return \ast\flags\USE_CONST;
        case PhpParser\Node\Statement\Use_::TYPE_UNKNOWN:
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

    private static function _phpparser_switch_list_to_ast_switch(PhpParser\Node\Statement\Switch_ $node) {
        $stmts = [];
        $nodeLine = self::getEndLine($node) ?? 0;
        foreach ($node->cases as $case) {
            $caseLine = self::getEndLine($case);
            $stmts[] = astnode(
                \ast\AST_SWITCH_CASE,
                0,
                [
                    'cond' => $case->cond !== null ? self::_phpparser_node_to_ast_node($case->cond) : null,
                    'stmts' => self::_phpparser_stmtlist_to_ast_node($case->statements, $caseLine),
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
        assert($node instanceof PhpParser\Node\Statement\If_);
        $startLine = self::getStartLine($node);
        $condLine = self::getEndLine($node->cond) ?: $startLine;
        $ifElem = self::_ast_if_elem(
            self::_phpparser_node_to_ast_node($node->cond),
            self::_phpparser_stmtlist_to_ast_node($node->statements, $condLine),
            $startLine
        );
        $ifElems = [$ifElem];
        foreach ($node->elseifs as $elseIf) {
            $ifElemLine = self::getStartLine($elseIf);
            $ifElem = self::_ast_if_elem(
                self::_phpparser_node_to_ast_node($elseIf->cond),
                self::_phpparser_stmtlist_to_ast_node($elseIf->statements, $ifElemLine),
                $ifElemLine
            );
            $ifElems[] = $ifElem;
        }
        $parserElseNode = $node->else;
        if ($parserElseNode) {
            $parserElseLine = self::getStartLine($parserElseNode);
            $ifElems[] = self::_ast_if_elem(
                null,
                self::_phpparser_stmtlist_to_ast_node($parserElseNode->statements, $parserElseLine),
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
                'expr' => self::_phpparser_node_to_ast_node($node->expression),
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

    private static function _phpparser_propelem_to_ast_propelem(PhpParser\Node\Statement\PropertyProperty $n, ?string $docComment) : \ast\Node{
        $children = [
            'name' => $n->name,
            'default' => $n->default ? self::_phpparser_node_to_ast_node($n->default) : null,
        ];

        $startLine = self::getStartLine($n);

        return astnode(\ast\AST_PROP_ELEM, 0, $children, $startLine, $n->getDocCommentText() ?? $docComment);
    }

    private static function _phpparser_constelem_to_ast_constelem(PhpParser\Node\ConstDeclaration $n, ?string $docComment) : \ast\Node{
        $children = [
            'name' => $n->name,
            'value' => self::_phpparser_node_to_ast_node($n->value),
        ];

        $startLine = self::getStartLine($n);

        return astnode(\ast\AST_CONST_ELEM, 0, $children, $startLine, $n->getDocCommentText() ?? $docComment);
    }

    /**
     * @param PhpParser\Token[] $visibility
     */
    private static function _phpparser_visibility_to_ast_visibility(array $visibility, bool $automatically_add_public = true) : int {
        $ast_visibility = 0;
        foreach ($visibility as $token) {
            $part = \strtolower(self::_token_to_string($token));
            switch($part) {
            case 'public':
                $ast_visibility |= \ast\flags\MODIFIER_PUBLIC;
                break;
            case 'protected':
                $ast_visibility |= \ast\flags\MODIFIER_PROTECTED;
                break;
            case 'private':
                $ast_visibility |= \ast\flags\MODIFIER_PRIVATE;
                break;
            case 'static':
                $ast_visibility |= \ast\flags\MODIFIER_STATIC;
                break;
            case 'abstract':
                $ast_visibility |= \ast\flags\MODIFIER_ABSTRACT;
                break;
            case 'final':
                $ast_visibility |= \ast\flags\MODIFIER_FINAL;
                break;
            default:
                throw new \RuntimeException("Unexpected visibility modifier '$part'");
            }
        }
        if ($automatically_add_public && !($ast_visibility & (\ast\flags\MODIFIER_PUBLIC|\ast\flags\MODIFIER_PROTECTED|\ast\flags\MODIFIER_PRIVATE))) {
            $ast_visibility |= \ast\flags\MODIFIER_PUBLIC;
        }
        return $ast_visibility;
    }

    private static function _phpparser_property_to_ast_node(PhpParser\Node $n, int $startLine) : \ast\Node {
        assert($n instanceof \PHPParser\Node\Statement\Property);

        $propElems = [];
        $docComment = $n->getDocCommentText();
        foreach ($n->props as $i => $prop) {
            $propElems[] = self::_phpparser_propelem_to_ast_propelem($prop, $i === 0 ? $docComment : null);
        }
        $flags = self::_phpparser_visibility_to_ast_visibility($n->flags);

        return astnode(\ast\AST_PROP_DECL, $flags, $propElems, $propElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_class_const_to_ast_node(PhpParser\Node\ClassConstDeclaration $n, int $startLine) : \ast\Node {
        $constElems = [];
        $docComment = $n->getDocCommentText();
        foreach ($n->consts as $i => $prop) {
            $constElems[] = self::_phpparser_constelem_to_ast_constelem($prop, $i === 0 ? $docComment : null);
        }
        $flags = self::_phpparser_visibility_to_ast_visibility($n->flags);

        return astnode(\ast\AST_CLASS_CONST_DECL, $flags, $constElems, $constElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_const_to_ast_node(PhpParser\Node\Statement\ConstDeclaration $n, int $startLine) : \ast\Node {
        $constElems = [];
        $docComment = $n->getDocCommentText();
        foreach ($n->consts as $i => $prop) {
            $constElems[] = self::_phpparser_constelem_to_ast_constelem($prop, $i === 0 ? $docComment : null);
        }

        return astnode(\ast\AST_CONST_DECL, 0, $constElems, $constElems[0]->lineno ?: $startLine);
    }

    private static function _phpparser_declare_directive_to_ast_declares(PhpParser\Node\DeclareDirective $declares, int $startLine) : \ast\Node {
        $astDeclareElements = [];
        foreach ($declares as $declare) {
            $children = [
                'name' => self::_token_to_string($declare->name),
                'value' => self::_token_to_scalar($declare->literal),
            ];
            $astDeclareElements[] = astnode(\ast\AST_CONST_ELEM, 0, $children, self::getStartLine($declare));
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

    private static function _phpparser_list_to_ast_list(PhpParser\Node\Expression\ArrayCreationExpression $n, int $startLine) : \ast\Node {
        $astItems = [];
        foreach ($n->items as $item) {
            if ($item === null) {
                $astItems[] = null;
            } else {
                $astItems[] = astnode(\ast\AST_ARRAY_ELEM, 0, [
                    'value' => self::_phpparser_node_to_ast_node($item->value),
                    'key' => $item->key !== null ? self::_phpparser_node_to_ast_node($item->key) : null,
                ], self::getStartLine($item));
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
                ], self::getStartLine($item));
            }
        }
        return astnode(\ast\AST_ARRAY, \ast\flags\ARRAY_SYNTAX_SHORT, $astItems, $startLine);
    }

    private static function _phpparser_propertyfetch_to_ast_prop(PhpParser\Node\Expression\MemberAccessExpression $n, int $startLine) : ?\ast\Node {
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

    private static function _token_to_scalar(PhpParser\Token $n) {
        $str = self::_token_to_string($n);
        $int = \filter_var($str, FILTER_VALIDATE_INT);
        if ($int !== false) {
            return $int;
        }
        // TODO: other cases
        return $str;
    }

    private static function _token_to_string(PhpParser\Token $n) : string {
        throw new RuntimeException("Not implemented yet");
    }

    /**
     * @param PhpParser\Node\Expression|PhpParser\Node\QualifiedName|PhpParser\Token $scopeResolutionQualifier
     */
    private static function _phpparser_classconstfetch_to_ast_classconstfetch($scopeResolutionQualifier, PhpParser\Token $name, int $startLine) : ?\ast\Node {
        $name = self::_phpparser_node_to_ast_node($name);
        // TODO: proper error handling of incomplete tokens?
        if ($name === null) {
            if (self::$should_add_placeholders) {
                $name = '__INCOMPLETE_CLASS_CONST__';
            } else {
                return null;
            }
        }
        return astnode(\ast\AST_CLASS_CONST, 0, [
            'class' => self::_phpparser_node_to_ast_node($scopeResolutionQualifier),
            'const' => $name,
        ], $startLine);
    }

    /**
     * @suppress PhanDeprecatedProperty TODO: figure out alternative
     */
    private static function _phpparser_name_to_string(PhpParser\Node\QualifiedName $name) : string {
        // TODO: Handle error case
        return implode('', array_map(function(PhpParser\Token $token) : string {
            return trim(self::_token_to_string($token));
        }, $name->parts));
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
