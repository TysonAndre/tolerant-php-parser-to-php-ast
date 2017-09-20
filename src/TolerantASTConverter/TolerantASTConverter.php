<?php declare(strict_types=1);

namespace TolerantASTConverter;

use ast;
use Microsoft\PhpParser;
use Microsoft\PhpParser\DiagnosticsProvider;
use Microsoft\PhpParser\Parser;
use Microsoft\PhpParser\TokenKind;

// If php-ast isn't loaded already, then load this file to generate equivalent
// class, constant, and function definitions.
if (!class_exists('\ast\Node')) {
    require_once __DIR__ . '/../ast_shim.php';
}


/**
 * Source: https://github.com/TysonAndre/tolerant-php-parser-to-php-ast
 * Uses Microsoft/tolerant-php-parser to create an instance of ast\Node.
 * Useful if the php-ast extension isn't actually installed.
 * @author Tyson Andre
 * TODO: Don't need to pass in $start_line for many of these functions
 *
 * This is implemented as a collection of static methods for performance,
 * but functionality is provided through instance methods.
 * (The private methods may become instance methods if the performance impact is negligible
 * in Zend PHP and HHVM)
 *
 * The instance methods set all of the options (static variables)
 * each time they are invoked,
 * so it's possible to have multiple callers use this without affecting each other.
 *
 * Compatibility: PHP 7.1 (7.0 is planned)
 *
 * ----------------------------------------------------------------------------
 *
 *
 * License for TolerantASTConverter.php:
 *
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
final class TolerantASTConverter {
    // The latest stable version of php-ast.
    // For something > 50, update the library's release.
    // For something < 40, there are no releases.
    const AST_VERSION = 50;

    // The versions that this supports
    const SUPPORTED_AST_VERSIONS = [40, 45, 50];

    /**
     * @var int - A version in SUPPORTED_AST_VERSIONS
     */
    private static $ast_version = self::AST_VERSION;

    /**
     * @var int - Internal counter for declarations, to generate __declId in `\ast\Node`s for declarations.
     */
    private static $decl_id = 0;

    /** @var bool */
    private static $should_add_placeholders = false;

    /** @var string */
    private static $file_contents = '';

    /** @var FilePositionMap */
    private static $file_position_map;

    /** @var bool Sets equivalent static option in self::_start_parsing() */
    private $instance_should_add_placeholders = false;

    // No-op.
    public function __construct() { }

    public static function setShouldAddPlaceholders(bool $value) : void {
        self::$should_add_placeholders = $value;
    }

    public function parseCodeAsPHPAST(string $file_contents, int $version, bool $suppress_errors = false, array &$errors = null) {
        if (!\in_array($version, self::SUPPORTED_AST_VERSIONS)) {
            throw new \InvalidArgumentException(sprintf("Unexpected version: want %d, got %d", implode(', ', self::SUPPORTED_AST_VERSIONS), $version));
        }
        // Aside: this can be implemented as a stub.
        $parser_node = self::phpParserParse($file_contents, $suppress_errors, $errors);
        return $this->phpParserToPhpast($parser_node, $version, $file_contents);
    }

    /**
     * @return PhpParser\Node
     */
    public static function phpParserParse(string $file_contents, bool $suppress_errors = false, array &$errors = null) : PhpParser\Node {
        $parser = new Parser();  // TODO: Language version?
        // $node_dumper = new PhpParser\NodeDumper();
        // TODO: Provide url
        $result = $parser->parseSourceFile($file_contents);
        $errors = DiagnosticsProvider::getDiagnostics($result);
        return $result;
    }


    /**
     * Visible for testing
     *
     * @param PhpParser\Node $parser_node
     * @param int $ast_version
     * @return ast\Node
     */
    public function phpParserToPhpast(PhpParser\Node $parser_node, int $ast_version, string $file_contents) {
        if (!\in_array($ast_version, self::SUPPORTED_AST_VERSIONS)) {
            throw new \InvalidArgumentException(sprintf("Unexpected version: want %s, got %d", implode(', ', self::SUPPORTED_AST_VERSIONS), $ast_version));
        }
        $this->startParsing($ast_version, $file_contents, $parser_node);
        $stmts = self::phpParserNodeToAstNode($parser_node);
        // return self::normalizeNamespaces($stmts);
        return $stmts;
    }

    /** @return void */
    private function startParsing(int $ast_version, string $file_contents, PhpParser\Node $parser_node) {
        self::$ast_version = $ast_version;
        self::$decl_id = 0;
        self::$should_add_placeholders = $this->instance_should_add_placeholders;
        self::$file_position_map = new FilePositionMap($file_contents, $parser_node);
        // $file_contents required for looking up line numbers.
        // TODO: Other data structures?
        self::$file_contents = $file_contents;
    }

    /**
     * @param PhpParser\Node[]|PhpParser\Node\StatementNode $parser_nodes
     * @param ?int $lineno
     */
    private static function phpParserStmtlistToAstNode($parser_nodes, $lineno) : ast\Node {
        if ($parser_nodes instanceof PhpParser\Node\Statement\CompoundStatementNode) {
            $parser_nodes = $parser_nodes->statements;
        } else if ($parser_nodes instanceof PhpParser\Node\Statement\EmptyStatement) {
            $parser_nodes = [];
        }
        if (!is_array($parser_nodes)) {
            throw new \RuntimeException("Unexpected type for statements: " . var_dump($parser_nodes, true));
        }
        $stmts = new ast\Node();
        $stmts->kind = ast\AST_STMT_LIST;
        $stmts->flags = 0;
        $children = [];
        foreach ($parser_nodes as $parser_node) {
            $child_node = self::phpParserNodeToAstNode($parser_node);
            if (is_array($child_node)) {
                // Echo_ returns multiple children.
                foreach ($child_node as $child_node_part) {
                    $children[] = $child_node_part;
                }
            } else if (!is_null($child_node)) {
                $children[] = $child_node;
            }
        }
        if (!is_int($lineno)) {
            foreach ($parser_nodes as $parser_node) {
                $child_node_line = self::getEndLine($parser_node);
                if ($child_node_line > 0) {
                    $lineno = $child_node_line;
                    break;
                }
            }
        }
        $stmts->lineno = $lineno ?? 0;
        $stmts->children = $children;
        return $stmts;
    }

    private static function phpParserExprListToExprList(PhpParser\Node\DelimitedList\ExpressionList $exprs, int $lineno) : ast\Node {
        $children = [];
        foreach ($exprs->children as $expr) {
            $child_node = self::phpParserNodeToAstNode($expr);
            if (is_array($child_node)) {
                // Echo_ returns multiple children.
                foreach ($child_node as $child_node_part) {
                    $children[] = $child_node_part;
                }
            } else if (!is_null($child_node)) {
                $children[] = $child_node;
            }
        }
        foreach ($exprs as $parser_node) {
            $child_node_line = self::getEndLine($parser_node);
            if ($child_node_line > 0) {
                $lineno = $child_node_line;
                break;
            }
        }
        return new ast\Node(
            ast\AST_EXPR_LIST,
            0,
            $children,
            $lineno
        );
    }

    /**
     * @param PhpParser\Node|PhpParser\Token $n - The node from PHP-Parser
     * @return ast\Node|ast\Node[]|string|int|float|bool|null - whatever ast\parse_code would return as the equivalent.
     * @suppress PhanUndeclaredProperty
     */
    private static final function phpParserNodeToAstNode($n) {
        if (!($n instanceof PhpParser\Node) && !($n instanceof PhpParser\Token)) {
            debug_print_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS);
            throw new \InvalidArgumentException("Invalid type for node: " . (is_object($n) ? get_class($n) : gettype($n)));
        }

        static $callback_map;
        static $fallback_closure;
        if (\is_null($callback_map)) {
            $callback_map = self::initHandleMap();
            $fallback_closure = function(PHPParser\Node $n, int $start_line) {
                return self::astStub($n);
            };
        }
        $callback = $callback_map[get_class($n)] ?? $fallback_closure;
        return $callback($n, self::getStartLine($n));
    }

    private static function getStartLine(?PhpParser\Node $n) : int {
        if (!$n) {
            return 0;
        }
        // TODO: binary search in an array mapping line number to character offset?
        // Currently returns character offset.
        return self::$file_position_map->getStartLine($n);
    }

    private static function getEndLine(?PhpParser\Node $n) : int {
        if (!$n) {
            return 0;
        }
        // TODO: binary search in an array mapping line number to character offset?
        // Currently returns character offset.
        return self::$file_position_map->getEndLine($n);
    }

    /**
     * This returns an array of values mapping class names to the closures which converts them to a scalar or ast\Node or ast\Node\Decl
     *
     * Why not a switch? Switches are slow until php 7.2, and there are dozens of class names to handle.
     *
     * - In php <= 7.1, the interpreter would loop through all possible cases, and compare against the value one by one.
     * - There are a lot of local variables to look at.
     *
     * @return \Closure[]
     */
    private static function initHandleMap() : array {
        $closures = [
            'Microsoft\PhpParser\Node\SourceFileNode' => function(PhpParser\Node\SourceFileNode $n, int $start_line) : ?\ast\Node {
                return self::phpParserStmtlistToAstNode($n->statementList, self::getStartLine($n) ?: 1);
            },
            'Microsoft\PhpParser\Node\Expression\ArgumentExpression' => function(PhpParser\Node\Expression\ArgumentExpression $n, int $start_line) {
                // FIXME support foo(...$args)
                return self::phpParserNodeToAstNode($n->expression/*, $n->dotDotdotToken */);
            },
            'Microsoft\PhpParser\Node\Expression\ArrayCreationExpression' => function(PhpParser\Node\Expression\ArrayCreationExpression $n, int $start_line) : ast\Node {
                return self::phpParserArrayToAstArray($n, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\SubscriptExpression' => function(PhpParser\Node\Expression\SubscriptExpression $n, int $start_line) : ast\Node {
                return new ast\Node(ast\AST_DIM, 0, [
                    'expr' => self::phpParserNodeToAstNode($n->postfixExpression),
                    'dim' => $n->accessExpression !== null ? self::phpParserNodeToAstNode($n->accessExpression) : null,
                ], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\AssignmentExpression' => function(PhpParser\Node\Expression\AssignmentExpression $n, int $start_line) : ?ast\Node {
                $op_kind = $n->operator->kind;
                assert($op_kind === TokenKind::EqualsToken);
                // FIXME switch on $n->kind
                return self::astNodeAssign(
                    self::phpParserNodeToAstNode($n->leftOperand),
                    self::phpParserNodeToAstNode($n->rightOperand),
                    $start_line,
                    $n->byRef !== null
                );
            },
            'Microsoft\PhpParser\Node\Expression\BinaryExpression' => function(PhpParser\Node\Expression\BinaryExpression $n, int $start_line) : ast\Node {
                static $lookup = [
                    TokenKind::AmpersandToken               => ast\flags\BINARY_BITWISE_AND,
                    TokenKind::BarToken                     => ast\flags\BINARY_BITWISE_OR,
                    TokenKind::CaretToken                   => ast\flags\BINARY_BITWISE_XOR,
                    TokenKind::AmpersandAmpersandToken      => ast\flags\BINARY_BOOL_AND,
                    TokenKind::BarBarToken                  => ast\flags\BINARY_BOOL_OR,
                    TokenKind::QuestionQuestionToken        => ast\flags\BINARY_COALESCE,
                ];
                $kind = $n->operator->kind;
                if ($kind === TokenKind::InstanceOfKeyword) {
                    return new ast\Node(ast\AST_INSTANCEOF, 0, [
                        'expr'  => self::phpParserNodeToAstNode($n->leftOperand),
                        'class' => self::phpParserNodeToAstNode($n->rightOperand),
                    ], $start_line);
                }
                $ast_kind = $lookup[$kind] ?? null;
                assert($ast_kind !== null, "missing $kind");
                return self::astNodeBinaryop($ast_kind, $n, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\UnaryOpExpression' => function(PhpParser\Node\Expression\UnaryOpExpression $n, int $start_line) : ast\Node {
                static $lookup = [
                    TokenKind::TildeToken                   => ast\flags\UNARY_BITWISE_NOT,
                    TokenKind::MinusToken                   => ast\flags\UNARY_MINUS,
                    TokenKind::PlusToken                    => ast\flags\UNARY_PLUS,
                ];
                $kind = $n->operator->kind;
                $ast_kind = $lookup[$kind] ?? null;
                assert($ast_kind !== null, "missing $kind");
                return self::astNodeUnaryOp($ast_kind, self::phpParserNodeToAstNode($n->operand), $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\CastExpression' => function(PhpParser\Node\Expression\CastExpression $n, int $start_line) : ast\Node {
                static $lookup = [
                    TokenKind::ArrayCastToken   => ast\flags\TYPE_ARRAY,
                    TokenKind::BoolCastToken    => ast\flags\TYPE_BOOL,
                    TokenKind::DoubleCastToken  => ast\flags\TYPE_DOUBLE,
                    TokenKind::IntCastToken     => ast\flags\TYPE_LONG,
                    TokenKind::ObjectCastToken  => ast\flags\TYPE_OBJECT,
                    TokenKind::StringCastToken  => ast\flags\TYPE_STRING,
                    TokenKind::DoubleCastToken  => ast\flags\TYPE_DOUBLE,
                    TokenKind::UnsetCastToken   => ast\flags\TYPE_VOID,
                ];
                $kind = $n->castType->kind;
                $ast_kind = $lookup[$kind] ?? null;
                assert($ast_kind !== null, "missing $kind");
                return self::astNodeCast($ast_kind, $n, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\AnonymousFunctionCreationExpression' => function(PhpParser\Node\Expression\AnonymousFunctionCreationExpression $n, int $start_line) : ast\Node {
                // TODO: is there a corresponding flag for $n->static? $n->byRef?
                return self::astDeclClosure(
                    $n->byRefToken !== null,
                    $n->staticModifier !== null,
                    self::phpParserParamsToAstParams($n->parameters, $start_line),
                    self::phpParserClosureUsesToAstClosureUses($n->anonymousFunctionUseClause->useVariableNameList, $start_line),
                    self::phpParserStmtlistToAstNode($n->compoundStatementOrSemicolon->statements, $start_line),
                    self::phpParserTypeToAstNode($n->returnType, self::getEndLine($n->returnType) ?: $start_line),
                    $start_line,
                    self::getEndLine($n),
                    $n->getDocCommentText()
                );
                // FIXME: add a test of ClassQualifiedName to php-ast
            },
            'Microsoft\PhpParser\Node\Expression\ScopedPropertyAccessExpression' => function(PhpParser\Node\Expression\ScopedPropertyAccessExpression $n, int $start_line) : ?ast\Node {
                $member_name = $n->memberName;
                if ($member_name instanceof PhpParser\Node\Expression\Variable) {
                    return new ast\Node(
                        ast\AST_STATIC_PROP,
                        0,
                        [
                            'class' => self::phpParserNodeToAstNode($n->scopeResolutionQualifier),
                            'prop' => self::phpParserNodeToAstNode($n->memberName),
                        ],
                        $start_line
                    );
                } else {
                    \assert($member_name instanceof PhpParser\Token);
                    return self::phpParserClassconstfetchToAstClassconstfetch($n->scopeResolutionQualifier, $member_name, $start_line);
                }
            },
            'Microsoft\PhpParser\Node\Expression\CloneExpression' => function(PhpParser\Node\Expression\CloneExpression $n, int $start_line) : ast\Node {
                return new ast\Node(ast\AST_CLONE, 0, ['expr' => self::phpParserNodeToAstNode($n->expression)], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\ErrorControlExpression' => function(PhpParser\Node\Expression\ErrorControlExpression $n, int $start_line) : ast\Node {
                return self::astNodeUnaryOp(ast\flags\UNARY_SILENCE, self::phpParserNodeToAstNode($n->operand), $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\EmptyIntrinsicExpression' => function(PhpParser\Node\Expression\EmptyIntrinsicExpression $n, int $start_line) : ast\Node {
                return new ast\Node(ast\AST_EMPTY, 0, ['expr' => self::phpParserNodeToAstNode($n->expression)], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\EvalIntrinsicExpression' => function(PhpParser\Node\Expression\EvalIntrinsicExpression $n, int $start_line) : ast\Node {
                return self::astNodeEval(
                    self::phpParserNodeToAstNode($n->expression),
                    $start_line
                );
            },
            'Microsoft\PhpParser\MissingToken' => function(PhpParser\MissingToken $n, int $start_line) {
                // This is where PhpParser couldn't parse a node.
                // TODO: handle this.
                return null;
            },
            'Microsoft\PhpParser\Node\Expression\ExitIntrinsicExpression' => function(PhpParser\Node\Expression\ExitIntrinsicExpression $n, int $start_line) {
                return new ast\Node(ast\AST_EXIT, 0, ['expr' => self::phpParserNodeToAstNode($n->expression)], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\CallExpression' => function(PhpParser\Node\Expression\CallExpression $n, int $start_line) : ast\Node {
                $callable_expression = $n->callableExpression;
                $arg_list = self::phpParserArgListToAstArgList($n->argumentExpressionList, $start_line);
                if ($callable_expression instanceof PhpParser\Node\Expression\MemberAccessExpression) {  // $a->f()
                    return self::astNodeMethodCall(
                        self::phpParserNodeToAstNode($callable_expression->dereferencableExpression),
                        self::phpParserNodeToAstNode($callable_expression->memberName),
                        $arg_list,
                        $start_line
                    );
                } else if ($callable_expression instanceof PhpParser\Node\Expression\ScopedPropertyAccessExpression) {  // a::f()
                    return self::astNodeStaticCall(
                        self::phpParserNodeToAstNode($callable_expression->scopeResolutionQualifier),
                        self::phpParserNodeToAstNode($callable_expression->memberName),
                        $arg_list,
                        $start_line
                    );
                } else {  // f()
                    return self::astNodeCall(
                        self::phpParserNodeToAstNode($callable_expression),
                        $arg_list,
                        $start_line
                    );
                }
            },
            'Microsoft\PhpParser\Node\Expression\ScriptInclusionExpression' => function(PhpParser\Node\Expression\ScriptInclusionExpression $n, int $start_line) : ast\Node {
                return self::astNodeInclude(
                    self::phpParserNodeToAstNode($n->expression),
                    $start_line,
                    $n->requireOrIncludeKeyword
                );
            },
            'Microsoft\PhpParser\Node\Expression\IssetIntrinsicExpression' => function(PhpParser\Node\Expression\IssetIntrinsicExpression $n, int $start_line) : ast\Node {
                $ast_issets = [];
                foreach ($n->expressions->children as $var) {
                    $ast_issets[] = new ast\Node(ast\AST_ISSET, 0, [
                        'var' => self::phpParserNodeToAstNode($var),
                    ], $start_line);
                }
                $e = $ast_issets[0];
                for ($i = 1; $i < \count($ast_issets); $i++) {
                    $right = $ast_issets[$i];
                    $e = new ast\Node(
                        ast\AST_BINARY_OP,
                        ast\flags\BINARY_BOOL_AND,
                        [
                            'left' => $e,
                            'right' => $right,
                        ],
                        $e->lineno
                    );
                }
                return $e;
            },
            'Microsoft\PhpParser\Node\Expression\ArrayCreationExpression' => function(PhpParser\Node\Expression\ArrayCreationExpression $n, int $start_line) : ast\Node {
                return self::phpParserListToAstList($n, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\ObjectCreationExpression' => function(PhpParser\Node\Expression\ObjectCreationExpression $n, int $start_line) : ast\Node {
                return new ast\Node(ast\AST_NEW, 0, [
                    'class' => self::phpParserNodeToAstNode($n->classTypeDesignator),
                    'args' => self::phpParserArgListToAstArgList($n->argumentExpressionList, $start_line),
                ], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\PrefixUpdateExpression' => function(PhpParser\Node\Expression\PrefixUpdateExpression $n, int $start_line) : ast\Node {
                $name = self::tokenToString($n->incrementOrDecrementOperator);
                switch ($name) {
                case '+': $type = ast\AST_PRE_INC; break;
                case '-': $type = ast\AST_PRE_DEC; break;
                default: throw new \RuntimeException('impossible operator ' . $type);
                }

                return new ast\Node($type, 0, ['var' => self::phpParserNodeToAstNode($n->operand)], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\PostfixUpdateExpression' => function(PhpParser\Node\Expression\PostfixUpdateExpression $n, int $start_line) : ast\Node {
                $name = self::tokenToString($n->incrementOrDecrementOperator);
                switch ($name) {
                case '+': $type = ast\AST_POST_INC; break;
                case '-': $type = ast\AST_POST_DEC; break;
                default: throw new \RuntimeException('impossible operator ' . $type);
                }

                return new ast\Node($type, 0, ['var' => self::phpParserNodeToAstNode($n->operand)], $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\PrintIntrinsicExpression' => function(PhpParser\Node\Expression\PrintIntrinsicExpression $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_PRINT,
                    0,
                    ['expr' => self::phpParserNodeToAstNode($n->expression)],
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Expression\MemberAccessExpression' => function(PhpParser\Node\Expression\MemberAccessExpression $n, int $start_line) : ?ast\Node {
                return self::phpParserMemberAccessExpressionToAstProp($n, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\TernaryExpression' => function(PhpParser\Node\Expression\TernaryExpression $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_CONDITIONAL,
                    0,
                    [
                        'cond' => self::phpParserNodeToAstNode($n->condition),
                        'true' => $n->ifExpression !== null ? self::phpParserNodeToAstNode($n->ifExpression) : null,
                        'false' => self::phpParserNodeToAstNode($n->elseExpression),
                    ],
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Expression\Variable' => function(PhpParser\Node\Expression\Variable $n, int $start_line) : ?ast\Node {
                return self::astNodeVariable($n->name, $start_line);
            },
            'Microsoft\PhpParser\Node\Expression\YieldExpression' => function(PhpParser\Node\Expression\YieldExpression $n, int $start_line) : ast\Node {
                $str = strtolower(self::tokenToString($n->yieldOrYieldFromKeyword));
                switch ($str) {
                case 'yield': $kind = ast\AST_YIELD;
                default:
                if (!preg_match('@^yield\s+from$@', $str)) {
                    throw new \RuntimeException('Invalid yield expression');
                }
                $kind = ast\AST_YIELD_FROM;
                }
                $array_element = $n->arrayElement;
                return new ast\Node(
                    $kind,
                    0,
                    [
                        'value' => $array_element->elementValue !== null ? self::phpParserNodeToAstNode($array_element->elementValue) : null,
                        'key'   => $array_element->elementKey   !== null ? self::phpParserNodeToAstNode($array_element->elementKey) : null,
                    ],
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\QualifiedName' => function(PhpParser\Node\QualifiedName $n, int $start_line) : ast\Node {
                $name = self::phpParserNameToString($n);
                return new ast\Node(
                    \ast\AST_NAME,
                    \ast\flags\NAME_NOT_FQ,
                    ['name' => $name],
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Parameter' => function(PhpParser\Node\Parameter $n, int $start_line) : ast\Node {
                $type_line = self::getEndLine($n->typeDeclaration) ?: $start_line;
                $default_line = self::getEndLine($n->typeDeclaration) ?: $type_line;
                return self::astNodeParam(
                    $n->questionToken !== null,
                    $n->byRefToken !== null,
                    $n->dotDotDotToken !== null,
                    self::phpParserTypeToAstNode($n->typeDeclaration, $type_line),
                    $n->variableName,
                    self::phpParserTypeToAstNode($n->default, $default_line),
                    $start_line
                );
            },
            /*
            // FIXME parse
            'Microsoft\PhpParser\Node\Scalar\Encapsed' => function(PhpParser\Node\Scalar\Encapsed $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_ENCAPS_LIST,
                    0,
                    array_map(function(PhpParser\Node $n) { return self::phpParserNodeToAstNode($n); }, $n->parts),
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Scalar\EncapsedStringPart' => function(PhpParser\Node\Scalar\EncapsedStringPart $n, int $start_line) : string {
                return $n->value;
            },
             */
            /** @return int|float */
            'Microsoft\PhpParser\Node\NumericLiteral' => function(PhpParser\Node\NumericLiteral $n, int $start_line) {
                $text = self::tokenToString($n->children);
                $as_int = filter_var($text, FILTER_VALIDATE_INT);
                if ($as_int !== false) {
                    return $as_int;
                }
                return (float)$text;
            },
            'Microsoft\PhpParser\Node\StringLiteral' => function(PhpParser\Node\StringLiteral $n, int $start_line) : string {
                // FIXME utilities to convert StringLiteral to string
                // FIXME build AST_ENCAPS_LIST
                $text = implode('', array_map(function($x) { return self::tokenToString($x); }, $n->children));
                // TODO: Handle shell_exec
                // TODO: Need a way to escape backslashes

                return $text;
            },
            /* FIXME QualifiedName
            'Microsoft\PhpParser\Node\Scalar\MagicConst\ClassDeclaration' => function(PhpParser\Node\Scalar\MagicConst\ClassDeclaration $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_CLASS, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Dir' => function(PhpParser\Node\Scalar\MagicConst\Dir $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_DIR, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\File' => function(PhpParser\Node\Scalar\MagicConst\File $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_FILE, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\FunctionDeclaration' => function(PhpParser\Node\Scalar\MagicConst\FunctionDeclaration $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_FUNCTION, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Line' => function(PhpParser\Node\Scalar\MagicConst\Line $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_LINE, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\Method' => function(PhpParser\Node\Scalar\MagicConst\Method $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_METHOD, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\NamespaceDefinition' => function(PhpParser\Node\Scalar\MagicConst\NamespaceDefinition $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_NAMESPACE, $start_line);
            },
            'Microsoft\PhpParser\Node\Scalar\MagicConst\TraitDeclaration' => function(PhpParser\Node\Scalar\MagicConst\TraitDeclaration $n, int $start_line) : ast\Node {
                return self::astMagicConst(ast\flags\MAGIC_TRAIT, $start_line);
            }, */
            'Microsoft\PhpParser\Node\Statement\ExpressionStatement' => function(PhpParser\Node\Statement\ExpressionStatement $n, int $start_line) : ?ast\Node {
                return self::phpParserNodeToAstNode($n->expression);
            },
            'Microsoft\PhpParser\Node\Statement\BreakOrContinueStatement' => function(PhpParser\Node\Statement\BreakOrContinueStatement $n, int $start_line) : ast\Node {
                switch ($n->breakOrContinueKeyword->length) {
                case 5: $kind = ast\AST_BREAK;
                case 8: $kind = ast\AST_CONTINUE;
                default: throw new \RuntimeException("Invalid BreakOrContinueStatement token");
                }
                return new ast\Node($kind, 0, ['depth' => isset($n->breakoutLevel) ? (int)self::tokenToString($n->breakoutLevel) : null], $start_line);
            },
            /*
            'Microsoft\PhpParser\Node\CatchClause' => function(PhpParser\Node\CatchClause $n, int $start_line) : ast\Node {
                // TODO: Change after https://github.com/Microsoft/tolerant-php-parser/issues/103 is supported
                return self::astStmtCatch(
                    self::phpParserNameListToAstNameList($n->qualifiedName, $start_line),
                    $n->var,
                    self::phpParserStmtlistToAstNode($n->compoundstatement->statements, $start_line),
                    $start_line
                );
            },
             */
            /*
            'Microsoft\PhpParser\Node\Statement\ClassDeclaration' => function(PhpParser\Node\Statement\ClassDeclaration $n, int $start_line) : ast\Node {
                $end_line = self::getEndLine($n) ?: $start_line;
                return self::astStmtClass(
                    self::phpParserClassModifierToAstClassFlags($n->abstractOrFinalModifier),
                    self::tokenToString($n->name),
                    $n->classBaseClause !== null? self::phpParserNameToString($n->classBaseClause->baseClass) : null,
                    $n->classInterfaceClause,
                    self::phpParserStmtlistToAstNode($n->classMembers, $start_line),
                    $start_line,
                    $end_line,
                    $n->getDocCommentText()
                );
            },
             */
            'Microsoft\PhpParser\Node\ClassConstDeclaration' => function(PhpParser\Node\ClassConstDeclaration $n, int $start_line) : ast\Node {
                return self::phpParserClassConstToAstNode($n, $start_line);
            },
            'Microsoft\PhpParser\Node\MethodDeclaration' => function(PhpParser\Node\MethodDeclaration $n, int $start_line) : ast\Node {
                return self::newAstDecl(
                    ast\AST_METHOD,
                    self::phpParserVisibilityToAstVisibility($n->modifiers) | ($n->byRefToken !== null ? ast\flags\RETURNS_REF : 0),
                    [
                        'params' => self::phpParserParamsToAstParams($n->parameters, $start_line),
                        'uses' => null,  // TODO: anonymous class?
                        'stmts' => is_array($n->compoundStatementOrSemicolon->statements) ? self::phpParserStmtlistToAstNode($n->compoundStatementOrSemicolon->statements, $start_line) : null,
                        'returnType' => self::phpParserTypeToAstNode($n->returnType, self::getEndLine($n->returnType) ?: $start_line)
                    ],
                    $start_line,
                    $n->getDocCommentText(),
                    $n->name,
                    self::getEndLine($n)
                );
            },
            'Microsoft\PhpParser\Node\Statement\ConstDeclaration' => function(PhpParser\Node\Statement\ConstDeclaration $n, int $start_line) : ast\Node {
                return self::phpParserConstToAstNode($n, $start_line);
            },
            'Microsoft\PhpParser\Node\Statement\DeclareStatement' => function(PhpParser\Node\Statement\DeclareStatement $n, int $start_line) : ast\Node {
                return self::astStmtDeclare(
                    self::phpParserDeclareListToAstDeclares($n->declareDirective, $start_line),
                    $n->statements !== null ? self::phpParserStmtlistToAstNode($n->statements, $start_line) : null,
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Statement\DoStatement' => function(PhpParser\Node\Statement\DoStatement $n, int $start_line) : ast\Node {
                return self::astNodeDoWhile(
                    self::phpParserNodeToAstNode($n->expression),
                    self::phpParserStmtlistToAstNode($n->statement, $start_line),
                    $start_line
                );
            },
            /**
             * @return ast\Node|ast\Node[]
             */
            'Microsoft\PhpParser\Node\Expression\EchoExpression' => function(PhpParser\Node\Expression\EchoExpression $n, int $start_line) {
                $ast_echos = [];
                foreach ($n->expressions as $expr) {
                    $ast_echos[] = self::astStmtEcho(
                        self::phpParserNodeToAstNode($expr),
                        $start_line
                    );
                }
                return count($ast_echos) === 1 ? $ast_echos[0] : $ast_echos;
            },
            'Microsoft\PhpParser\Node\Statement\ForeachStatement' => function(PhpParser\Node\Statement\ForeachStatement $n, int $start_line) : ast\Node {
                $value = self::phpParserNodeToAstNode($n->foreachValue);
                $foreachValue = $n->foreachValue;
                if ($foreachValue->ampersand) {
                    $value = new ast\Node(
                        ast\AST_REF,
                        0,
                        ['var' => $value],
                        $value->lineno ?? $start_line
                    );
                }
                return new ast\Node(
                    ast\AST_FOREACH,
                    0,
                    [
                        'expr' => self::phpParserNodeToAstNode($foreachValue->expression),
                        'value' => self::phpParserNodeToAstNode($n->forEachCollectionName),
                        'key' => $n->foreachKey !== null ? self::phpParserNodeToAstNode($n->foreachKey) : null,
                        'stmts' => self::phpParserStmtlistToAstNode($n->statements, $start_line),
                    ],
                    $start_line
                );
                //return self::phpParserStmtlistToAstNode($n->statements, $start_line);
            },
            'Microsoft\PhpParser\Node\FinallyClause' => function(PhpParser\Node\FinallyClause $n, int $start_line) : ast\Node {
                return self::phpParserStmtlistToAstNode($n->compoundStatement, $start_line);
            },
            'Microsoft\PhpParser\Node\Statement\FunctionDeclaration' => function(PhpParser\Node\Statement\FunctionDeclaration $n, int $start_line) : ast\Node {
                $end_line = self::getEndLine($n) ?: $start_line;
                $return_type = $n->returnType;
                $return_type_line = ($return_type ? self::getEndLine($return_type) : 0) ?: $end_line;
                $ast_return_type = self::phpParserTypeToAstNode($return_type, $return_type_line);

                return self::astDeclFunction(
                    $n->byRefToken !== null,
                    self::tokenToString($n->name),
                    self::phpParserParamsToAstParams($n->parameters, $start_line),
                    null,  // uses
                    self::phpParserTypeToAstNode($return_type, $return_type_line),
                    self::phpParserStmtlistToAstNode($n->compoundStatementOrSemicolon, $start_line),
                    $start_line,
                    $end_line,
                    $n->getDocCommentText()
                );
            },
            /** @return ast\Node|ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\GlobalDeclaration' => function(PhpParser\Node\Statement\GlobalDeclaration $n, int $start_line) {
                $global_nodes = [];
                foreach ($n->variableNameList as $var) {
                    $global_nodes[] = new ast\Node(ast\AST_GLOBAL, 0, ['var' => self::phpParserNodeToAstNode($var)], self::getEndLine($var) ?: $start_line);
                }
                return \count($global_nodes) === 1 ? $global_nodes[0] : $global_nodes;
            },
            'Microsoft\PhpParser\Node\Statement\IfStatementNode' => function(PhpParser\Node\Statement\IfStatementNode $n, int $start_line) : ast\Node {
                return self::phpParserIfStmtToAstIfStmt($n);
            },
            /** @return ast\Node|ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\InlineHtml' => function(PhpParser\Node\Statement\InlineHtml $n, int $start_line) {
                return $n->text !== null ? self::astStmtEcho($n->text, $start_line) : [];
            },
            /*
Node\SourceFileNode
    statementList: Node\Statement\InterfaceDeclaration
        interfaceKeyword: Token: InterfaceKeyword(143): "interface"
        name: Token: Name(2): " X"
        interfaceMembers: Node\InterfaceMembers
            openBrace: Token: OpenBraceToken(205): "{"
            closeBrace: Token: CloseBraceToken(206): "}"
    endOfFileToken: Token: EndOfFileToken(1): ""

            'Microsoft\PhpParser\Node\Statement\InterfaceDeclaration' => function(PhpParser\Node\Statement\InterfaceDeclaration $n, int $start_line) : ast\Node {
                $end_line = self::getEndLine($n) ?: $start_line;
                // FIXME: interface extending other interface
                return self::astStmtClass(
                    ast\flags\CLASS_INTERFACE,
                    self::tokenToString($n->name),
                    null,
                    null,
                    is_array($n->statements) ? self::phpParserStmtlistToAstNode($n->statements, $start_line) : null,
                    $start_line,
                    $end_line,
                    $n->getDocCommentText()
                );
            },
Node\SourceFileNode
             */
            /** @suppress PhanTypeMismatchArgument TODO: Make ForStatement have more accurate docs? */
            'Microsoft\PhpParser\Node\Statement\ForStatement' => function(PhpParser\Node\Statement\ForStatement $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_FOR,
                    0,
                    [
                        'init' => $n->forInitializer !== null ? self::phpParserExprListToExprList($n->forInitializer, $start_line) : null,
                        'cond' => $n->forControl !== null     ? self::phpParserExprListToExprList($n->forControl, $start_line) : null,
                        'loop' => $n->forEndOfLoop !== null   ? self::phpParserExprListToExprList($n->forEndOfLoop, $start_line) : null,
                        'stmts' => self::phpParserStmtlistToAstNode($n->statements, $start_line),
                    ],
                    $start_line
                );
            },
            /** @return ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\NamespaceUseDeclaration' => function(PhpParser\Node\Statement\NamespaceUseDeclaration $n, int $start_line) {
                $useClauses = $n->useClauses;
                $results = [];
                foreach ($useClauses->children as $useClause) {
                    \assert($useClause instanceof PhpParser\Node\NamespaceUseClause);
                    $results[] = self::astStmtGroupUse(
                        $n->functionOrConst->kind,  // E.g. kind is FunctionKeyword or ConstKeyword
                        self::phpParserNameToString($useClause->namespaceName),
                        [],  // FIXME self::phpParserUseListToAstUseList($useClause->groupClauses),
                        $start_line
                    );
                }
                return $results;
            },
            'Microsoft\PhpParser\Node\Statement\NamespaceDefinition' => function(PhpParser\Node\Statement\NamespaceDefinition $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_NAMESPACE,
                    0,
                    [
                        'name' => $n->name !== null ? self::phpParserNameToString($n->name) : null,
                        'stmts' => ($n->compoundStatementOrSemicolon instanceof PhpParser\Node)  ? self::phpParserStmtlistToAstNode($n->compoundStatementOrSemicolon, $start_line) : null,
                    ],
                    $start_line
                );
            },
            'Microsoft\PhpParser\Node\Statement\EmptyStatement' => function(PhpParser\Node\Statement\EmptyStatement $n, int $start_line) : array {
                // `;;`
                return [];
            },
            'Microsoft\PhpParser\Node\PropertyDeclaration' => function(PhpParser\Node\PropertyDeclaration $n, int $start_line) : ast\Node {
                return self::phpParserPropertyToAstNode($n, $start_line);
            },
            'Microsoft\PhpParser\Node\Statement\ReturnStatement' => function(PhpParser\Node\Statement\ReturnStatement $n, int $start_line) : ast\Node {
                return self::astStmtReturn($n->expression !== null ? self::phpParserNodeToAstNode($n->expression) : null, $start_line);
            },
            /** @return ast\Node|ast\Node[] */
            'Microsoft\PhpParser\Node\Statement\FunctionStaticDeclaration' => function(PhpParser\Node\Statement\FunctionStaticDeclaration $n, int $start_line) {
                $static_nodes = [];
                foreach ($n->staticVariableNameList as $var) {
                    assert($var instanceof PhpParser\Node\StaticVariableDeclaration);
                    $static_nodes[] = new ast\Node(ast\AST_STATIC, 0, [
                        'var' => new ast\Node(ast\AST_VAR, 0, ['name' => self::phpParserNodeToAstNode($var->variableName)], self::getEndLine($var) ?: $start_line),
                        'default' => $var->assignment !== null ? self::phpParserNodeToAstNode($var->assignment) : null,
                    ], self::getEndLine($var) ?: $start_line);
                }
                return \count($static_nodes) === 1 ? $static_nodes[0] : $static_nodes;
            },
            'Microsoft\PhpParser\Node\Statement\SwitchStatementNode' => function(PhpParser\Node\Statement\SwitchStatementNode $n, int $start_line) : ast\Node {
                return self::phpParserSwitchListToAstSwitch($n);
            },
            'Microsoft\PhpParser\Node\Statement\ThrowStatement' => function(PhpParser\Node\Statement\ThrowStatement $n, int $start_line) : ast\Node {
                return new ast\Node(
                    ast\AST_THROW,
                    0,
                    ['expr' => self::phpParserNodeToAstNode($n->expression)],
                    $start_line
                );
            },

            /**
            'Microsoft\PhpParser\Node\Statement\TraitDeclaration' => function(PhpParser\Node\Statement\TraitDeclaration $n, int $start_line) : ast\Node {
                $end_line = self::getEndLine($n) ?: $start_line;
                return self::astStmtClass(
                    ast\flags\CLASS_TRAIT,
                    $n->name,
                    null,
                    null,
                    self::phpParserStmtlistToAstNode($n->statements, $start_line),
                    $start_line,
                    $end_line,
                    $n->getDocCommentText()
                );
            },
             */
            'Microsoft\PhpParser\Node\TraitUseClause' => function(PhpParser\Node\TraitUseClause $n, int $start_line) : ast\Node {
                if ($n->traitSelectAndAliasClauses instanceof PhpParser\Node\DelimitedList\TraitSelectOrAliasClauseList) {
                    $adaptations_inner = array_map(function(PhpParser\Node\TraitSelectOrAliasClause $n) : ast\Node {
                        return self::phpParserNodeToAstNode($n);
                    }, $n->traitSelectAndAliasClauses->children);
                    $adaptations = new ast\Node(ast\AST_TRAIT_ADAPTATIONS, 0, $adaptations_inner, $adaptations_inner[0]->lineno ?: $start_line);
                } else {
                    $adaptations = null;
                }
                return new ast\Node(
                    ast\AST_USE_TRAIT,
                    0,
                    [
                        'traits' => self::phpParserNameListToAstNameList($n->traitNameList->children ?? [], $start_line),
                        'adaptations' => $adaptations,
                    ],
                    $start_line
                );
            },
    /*

     class C{use X,Y{X::foo insteadof Y}
Node\SourceFileNode
    statementList: Node\Statement\ClassDeclaration
        classKeyword: Token: ClassKeyword(109): "class"
        name: Token: Name(2): " C"
        classMembers: Node\ClassMembersNode
            openBrace: Token: OpenBraceToken(205): "{"
            classMemberDeclarations: Node\TraitUseClause
                useKeyword: Token: UseKeyword(162): "use"
                traitNameList: Node\DelimitedList\QualifiedNameList
                    children: Node\QualifiedName
                        nameParts: Token: Name(2): " X"
                    children: Token: CommaToken(251): ","
                    children: Node\QualifiedName
                        nameParts: Token: Name(2): "Y"
                semicolonOrOpenBrace: Token: OpenBraceToken(205): "{"
                traitSelectAndAliasClauses: Node\DelimitedList\TraitSelectOrAliasClauseList
                    children: Node\TraitSelectOrAliasClause
                        name: Node\Expression\ScopedPropertyAccessExpression
                            scopeResolutionQualifier: Node\QualifiedName
                                nameParts: Token: Name(2): "X"
                            doubleColon: Token: ColonColonToken(256): "::"
                            memberName: Token: Name(2): "foo"
                        asOrInsteadOfKeyword: Token: InsteadOfKeyword(142): " insteadof"
                        targetName: Node\QualifiedName
                            nameParts: Token: Name(2): " Y"
                closeBrace: Token: CloseBraceToken(206): "}"
            closeBrace: Token: CloseBraceToken(206): ""
    statementList: Node\Statement\EmptyStatement
        semicolon: Token: SemicolonToken(237): ";"
    statementList: Token: CloseBraceToken(206): " }"
    endOfFileToken: Token: EndOfFileToken(1): ""

            'Microsoft\PhpParser\Node\TraitSelectOrAliasClause' => function(PhpParser\Node\TraitSelectOrAliasClause $n, int $start_line) : ast\Node {
                //
                $old_class = $n->trait !== null ? self::phpParserNodeToAstNode($n->trait) : null;
                $flags = ($n->trait instanceof PhpParser\Node\Name\FullyQualified) ? ast\flags\NAME_FQ : ast\flags\NAME_NOT_FQ;
                // TODO: flags for visibility
                return new ast\Node(ast\AST_TRAIT_ALIAS, self::phpParserVisibilityToAstVisibility($n->newModifier ?? 0, false), [
                    'method' => new ast\Node(ast\AST_METHOD_REFERENCE, 0, [
                        'class' => $old_class,
                        'method' => $n->method,
                    ], $start_line),
                    'alias' => $n->newName,
                ], $start_line);
            },
            'Microsoft\PhpParser\Node\Statement\TraitUseAdaptation\Precedence' => function(PhpParser\Node\Statement\TraitUseAdaptation\Precedence $n, int $start_line) : ast\Node {
                $old_class = $n->trait !== null ? self::phpParserNameToString($n->trait) : null;
                $flags = ($n->trait instanceof PhpParser\Node\Name\FullyQualified) ? ast\flags\NAME_FQ : ast\flags\NAME_NOT_FQ;
                // TODO: flags for visibility
                return new ast\Node(ast\AST_TRAIT_PRECEDENCE, 0, [
                    'method' => new ast\Node(ast\AST_METHOD_REFERENCE, 0, [
                        'class' => new ast\Node(ast\AST_NAME, $flags, ['name' => $old_class], $start_line),
                        'method' => $n->method,
                    ], $start_line),
                    'insteadof' => self::phpParserNameListToAstNameList($n->insteadof, $start_line),
                ], $start_line);
            },
     */
            'Microsoft\PhpParser\Node\Statement\TryStatement' => function(PhpParser\Node\Statement\TryStatement $n, int $start_line) : ast\Node {
                if (!is_array($n->catchClauses)) {
                    throw new \Error(sprintf("Unsupported type %s\n%s", get_class($n), var_dump($n->catchClauses, true)));
                }
                return self::astNodeTry(
                    self::phpParserStmtlistToAstNode($n->compoundStatement, $start_line), // $n->try
                    self::phpParserCatchlistToAstCatchlist($n->catchClauses, $start_line),
                    isset($n->finallyClause) ? self::phpParserStmtlistToAstNode($n->finallyClause->compoundStatement, self::getEndLine($n->finallyClause)) : null,
                    $start_line
                );
            },
            /** @return ast\Node|ast\Node[] */
            'Microsoft\PhpParser\Node\Expression\UnsetIntrinsicExpression' => function(PhpParser\Node\Expression\UnsetIntrinsicExpression $n, int $start_line) {
                $stmts = [];
                foreach ($n->expressions->children as $var) {
                    $stmts[] = new ast\Node(ast\AST_UNSET, 0, ['var' => self::phpParserNodeToAstNode($var)], self::getEndLine($var) ?: $start_line);
                }
                return \count($stmts) === 1 ? $stmts[0] : $stmts;
            },
            'Microsoft\PhpParser\Node\Statement\WhileStatement' => function(PhpParser\Node\Statement\WhileStatement $n, int $start_line) : ast\Node {
                return self::astNodeWhile(
                    self::phpParserNodeToAstNode($n->expression),
                    self::phpParserStmtlistToAstNode($n->statements, $start_line),
                    $start_line
                );
            },
        ];

        foreach ($closures as $key => $value) {
            assert(class_exists($key), "Class $key should exist");
        }
        return $closures;
    }

    private static function astNodeTry(
        $try_node,
        $catches_node,
        $finally_node,
        int $start_line
    ) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_TRY;
        $node->flags = 0;
        $node->lineno = $start_line;
        $children = [
            'try' => $try_node,
        ];
        if ($catches_node !== null) {
            $children['catches'] = $catches_node;
        }
        $children['finally'] = $finally_node;
        $node->children = $children;
        return $node;
    }

    // FIXME types
    private static function astStmtCatch($types, string $var, $stmts, int $lineno) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_CATCH;
        $node->lineno = $lineno;
        $node->flags = 0;
        $node->children = [
            'class' => $types,
            'var' => new ast\Node(ast\AST_VAR, 0, ['name' => $var], end($types->children)->lineno),  // FIXME AST_VAR
            'stmts' => $stmts,
        ];
        return $node;
    }

    private static function phpParserCatchlistToAstCatchlist(array $catches, int $lineno) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_CATCH_LIST;
        $node->flags = 0;
        $children = [];
        foreach ($catches as $parser_catch) {
            $children[] = self::phpParserNodeToAstNode($parser_catch);
        }
        $node->lineno = $children[0]->lineno ?? $lineno;
        $node->children = $children;
        return $node;
    }

    private static function phpParserNameListToAstNameList(array $types, int $line) : ast\Node {
        $ast_types = [];
        foreach ($types as $type) {
            $ast_types[] = self::phpParserNodeToAstNode($type);
        }
        return new ast\Node(ast\AST_NAME_LIST, 0, $ast_types, $line);
    }

    private static function astNodeWhile($cond, $stmts, int $start_line) : ast\Node {
        return new ast\Node(
            ast\AST_WHILE,
            0,
            [
                'cond' => $cond,
                'stmts' => $stmts,
            ],
            $start_line
        );
    }

    private static function astNodeDoWhile($cond, $stmts, int $start_line) : ast\Node {
        return new ast\Node(
            ast\AST_DO_WHILE,
            0,
            [
                'stmts' => $stmts,
                'cond' => $cond,
            ],
            $start_line
        );
    }

    private static function astNodeAssign($var, $expr, int $line, bool $ref) : ?ast\Node {
        if ($expr === null) {
            if (self::$should_add_placeholders) {
                $expr = '__INCOMPLETE_EXPR__';
            } else {
                return null;
            }
        }
        $node = new ast\Node();
        $node->kind = $ref ? ast\AST_ASSIGN_REF : ast\AST_ASSIGN;
        $node->flags = 0;
        $node->children = [
            'var'  => $var,
            'expr' => $expr,
        ];
        $node->lineno = $line;
        return $node;
    }

    private static function astNodeUnaryOp(int $flags, $expr, int $line) : ast\Node {
        return new ast\Node(ast\AST_UNARY_OP, $flags, ['expr' => $expr], $line);
    }

    private static function astNodeCast(int $flags, PhpParser\Node\Expression\CastExpression $n, int $line) : ast\Node {
        return new ast\Node(ast\AST_CAST, $flags, ['expr' => self::phpParserNodeToAstNode($n->operand)], self::getEndLine($n) ?: $line);
    }

    private static function astNodeEval($expr, int $line) : ast\Node {
        return new ast\Node(ast\AST_INCLUDE_OR_EVAL, ast\flags\EXEC_EVAL, ['expr' => $expr], $line);
    }

    private static function phpParserIncludeTokenToAstIncludeFlags(PhpParser\Token $type) : int {
        $type_name = strtolower(self::tokenToString($type));
        switch($type_name) {
        case 'include':
            return ast\flags\EXEC_INCLUDE;
        case 'include_once':
            return ast\flags\EXEC_INCLUDE_ONCE;
        case 'require':
            return ast\flags\EXEC_REQUIRE;
        case 'require_once':
            return ast\flags\EXEC_REQUIRE_ONCE;
        default:
            throw new \Error("Unrecognized PhpParser include/require type $type_name");
        }
    }
    private static function astNodeInclude($expr, int $line, PhpParser\Token $type) : ast\Node {
        $flags = self::phpParserIncludeTokenToAstIncludeFlags($type);
        return new ast\Node(ast\AST_INCLUDE_OR_EVAL, $flags, ['expr' => $expr], $line);
    }

    /**
     * @param PhpParser\Node|null $type
     * @return ast\Node|null
     */
    private static function phpParserTypeToAstNode($type, int $line) {
        if (is_null($type)) {
            return $type;
        }
        if (\is_string($type)) {
            switch(strtolower($type)) {
            case 'null':
                $flags = ast\flags\TYPE_NULL; break;
            case 'bool':
                $flags = ast\flags\TYPE_BOOL; break;
            case 'int':
                $flags = ast\flags\TYPE_LONG; break;
            case 'float':
                $flags = ast\flags\TYPE_DOUBLE; break;
            case 'string':
                $flags = ast\flags\TYPE_STRING; break;
            case 'array':
                $flags = ast\flags\TYPE_ARRAY; break;
            case 'object':
                if (self::$ast_version >= 45) {
                    $flags = ast\flags\TYPE_OBJECT; break;
                } else {
                    return new ast\Node(
                        ast\AST_NAME,
                        substr($type, 0, 1) === '\\' ? ast\flags\NAME_FQ : ast\flags\NAME_NOT_FQ,  // FIXME wrong.
                        [
                            'name' => $type,
                        ],
                        $line
                    );
                }
            case 'callable':
                $flags = ast\flags\TYPE_CALLABLE; break;
            case 'void':
                $flags = ast\flags\TYPE_VOID; break;
            case 'iterable':
                $flags = ast\flags\TYPE_ITERABLE; break;
            default:
                return new ast\Node(
                    ast\AST_NAME,
                    substr($type, 0, 1) === '\\' ? ast\flags\NAME_FQ : ast\flags\NAME_NOT_FQ,  // FIXME wrong.
                    [
                        'name' => $type,
                    ],
                    $line
                );
            }
            $node = new ast\Node();
            $node->kind = ast\AST_TYPE;
            $node->flags = $flags;
            $node->lineno = $line;
            $node->children = [];
            return $node;
        }
        return self::phpParserNodeToAstNode($type);
    }

    /**
     * @param bool $by_ref
     * @param ?ast\Node $type
     */
    private static function astNodeParam(bool $is_nullable, bool $by_ref, bool $variadic, $type, $name, $default, int $line) : ast\Node {
        $node = new ast\Node;
        $node->kind = ast\AST_PARAM;
        $node->flags = ($by_ref ? ast\flags\PARAM_REF : 0) | ($variadic ? ast\flags\PARAM_VARIADIC : 0);
        $node->lineno = $line;
        $node->children = [
            'type' => $type,
            'name' => $name,
            'default' => $default,
        ];
        if ($is_nullable) {
            return self::astNodeNullableType(
                $node,
                $line
            );
        }

        return $node;
    }

    private static function astNodeNullableType(ast\Node $type, int $line) {
        $node = new ast\Node;
        $node->kind = ast\AST_NULLABLE_TYPE;
        // FIXME: Why is this a special case in php-ast? (e.g. nullable int has no flags on the nullable node)
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['type' => $type];
        return $node;
    }

    private static function astNodeName(PhpParser\Node\QualifiedName $name, int $line) : ast\Node {
        // FIXME: skip over whitespace and \\
        $imploded_parts = self::phpParserNameToString($name);
        if ($name->globalSpecifier !== null) {
            return new ast\Node(ast\AST_NAME, ast\flags\NAME_FQ, ['name' => $imploded_parts], $line);
        }
        return new ast\Node(ast\AST_NAME, ast\flags\NAME_NOT_FQ, ['name' => $imploded_parts], $line);
    }

    /**
     * @param PhpParser\Node|PhpParser\Token $expr
     */
    private static function astNodeVariable($expr, int $line) : ?ast\Node {
        // TODO: 2 different ways to handle an Error. 1. Add a placeholder. 2. remove all of the statements in that tree.
        if ($expr instanceof PhpParser\Node) {
            $expr = self::phpParserNodeToAstNode($expr);
            if ($expr === null) {
                if (self::$should_add_placeholders) {
                    $expr = '__INCOMPLETE_VARIABLE__';
                } else {
                    return null;
                }
            }
        } else if ($expr instanceof PhpParser\Token) {
            if ($expr instanceof PhpParser\MissingToken) {
                if (self::$should_add_placeholders) {
                    $expr = '__INCOMPLETE_VARIABLE__';
                } else {
                    return null;
                }
            } else {
                $expr = self::tokenToString($expr);
            }
        }
        $node = new ast\Node;
        $node->kind = ast\AST_VAR;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['name' => $expr];
        return $node;
    }

    private static function astMagicConst(int $flags, int $line) {
        return new ast\Node(ast\AST_MAGIC_CONST, $flags, [], $line);
    }

    private static function phpParserParamsToAstParams(?PhpParser\Node\DelimitedList\ParameterDeclarationList $parser_params, int $line) : ast\Node {
        $new_params = [];
        foreach ($parser_params->children ?? [] as $parser_node) {
            $new_params[] = self::phpParserNodeToAstNode($parser_node);
        }
        $new_params_node = new ast\Node();
        $new_params_node->kind = ast\AST_PARAM_LIST;
        $new_params_node->flags = 0;
        $new_params_node->children = $new_params;
        $new_params_node->lineno = $line;
        return $new_params_node;
    }

    /**
     * @suppress PhanTypeMismatchProperty - Deliberately wrong type of kind
     */
    private static function astStub($parser_node) : ast\Node{
        // Debugging code.
        if (getenv('AST_THROW_INVALID')) {
            throw new \Error("TODO:" . get_class($parser_node));
        }

        $node = new ast\Node();
        $node->kind = "TODO:" . get_class($parser_node);
        $node->flags = 0;
        $node->lineno = self::getStartLine($parser_node);
        $node->children = null;
        return $node;
    }

    /**
     * @param PhpParser\Node\DelimitedList\UseVariableNameList $uses
     * @param int $line
     * @return ?ast\Node
     */
    private static function phpParserClosureUsesToAstClosureUses(
        PhpParser\Node\DelimitedList\UseVariableNameList $uses,
        int $line
    ) {
        if (count($uses->children) === 0) {
            return null;
        }
        $ast_uses = [];
        foreach ($uses->children as $use) {
            assert($use instanceof PhpParser\Node\UseVariableName);
            $ast_uses[] = new ast\Node(ast\AST_CLOSURE_VAR, $use->byRef ? 1 : 0, ['name' => self::tokenToString($use->variableName)], self::getStartLine($use));
        }
        return new ast\Node(ast\AST_CLOSURE_USES, 0, $ast_uses, $ast_uses[0]->lineno ?? $line);

    }

    private static function astDeclClosure(
        bool $by_ref,
        bool $static,
        ast\Node $params,
        $uses,
        $stmts,
        $return_type,
        int $start_line,
        int $end_line,
        ?string $doc_comment
    ) : ast\Node\Decl {
        return self::newAstDecl(
            ast\AST_CLOSURE,
            ($by_ref ? ast\flags\RETURNS_REF : 0) | ($static ? ast\flags\MODIFIER_STATIC : 0),
            [
                'params' => $params,
                'uses' => $uses,
                'stmts' => $stmts,
                'returnType' => $return_type,
            ],
            $start_line,
            $doc_comment,
            '{closure}',
            $end_line,
            self::nextDeclId()
        );
    }

    /**
     * @suppress PhanTypeMismatchProperty
     */
    private static function astDeclFunction(
        bool $by_ref,
        string $name,
        ast\Node $params,
        ?array $uses,
        $return_type,
        $stmts,
        int $line,
        int $end_line,
        ?string $doc_comment
    ) : ast\Node\Decl {
        return self::newAstDecl(
            ast\AST_FUNC_DECL,
            0,
            [
                'params' => $params,
                'uses' => $uses,
                'stmts' => $stmts,
                'returnType' => $return_type,
            ],
            $line,
            $doc_comment,
            $name,
            $end_line,
            self::nextDeclId()
        );
    }

    private static function phpParserClassModifierToAstClassFlags(?PhpParser\Token $flags) : int {
        if ($flags === null) {
            return 0;
        }
        $name = strtolower(self::tokenToString($flags));
        $ast_flags = 0;
        if ($name === 'abstract') {
            $ast_flags |= ast\flags\CLASS_ABSTRACT;
        }
        if ($flags === 'final') {
            $ast_flags |= ast\flags\CLASS_FINAL;
        }
        return $ast_flags;
    }

    /**
     * @param int $flags
     * @param ?string $name
     * @param ?string $extends TODO
     * @param ?PhpParser\Node\ClassInterfaceClause $implements
     * @param ?ast\Node $stmts
     * @param int $line
     * @param int $end_line
     * @suppress PhanTypeMismatchProperty (?string to string|null is incorrectly reported)
     */
    private static function astStmtClass(
        int $flags,
        ?string $name,
        ?string $extends,
        ?PhpParser\Node\ClassInterfaceClause $implements,
        ?ast\Node $stmts,
        int $line,
        int $end_line,
        ?string $doc_comment
    ) : ast\Node {
        if ($name === null) {
            $flags |= ast\flags\CLASS_ANONYMOUS;
        }

        if ($implements !== null) {
            $ast_implements_inner = [];
            foreach ($implements as $implement) {
                $ast_implements_inner[] = self::phpParserNodeToAstNode($implement);
            }
            $ast_implements = new ast\Node(ast\AST_NAME_LIST, 0, $ast_implements_inner, $ast_implements_inner[0]->lineno);
        } else {
            $ast_implements = null;
        }

        return self::newAstDecl(
            ast\AST_CLASS,
            $flags,
            [
                'extends'    => $extends,
                'implements' => $ast_implements,
                'stmts'      => $stmts,
            ],
            $line,
            $doc_comment,
            $name,
            $end_line
        );
    }

    private static function phpParserArgListToAstArgList(?PhpParser\Node\DelimitedList\ArgumentExpressionList $args, int $line) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_ARG_LIST;
        $node->flags = 0;
        $ast_args = [];
        foreach ($args->children ?? [] as $arg) {
            $ast_args[] = self::phpParserNodeToAstNode($arg);
        }
        $node->lineno = $ast_args[0]->lineno ?? $line;
        $node->children = $ast_args;
        return $node;
    }

    // FIXME implement this.
    private static function phpParserNamespaceUseKindToASTUseFlags(int $kind) : int {
        return 0;
    }

    /**
     * @param PhpParser\Node\UseVariableName[] $uses (FIXME clarify naming)
     * @return ast\Node[]
    private static function phpParserUseListToAstUseList(array $uses) : array {
        $ast_uses = [];
        foreach ($uses as $use) {
            $ast_use = new ast\Node();
            $ast_use->kind = ast\AST_USE_ELEM;
            $ast_use->flags = 0;  // self::phpParserUseTypeToAstFlags($use->type);  // FIXME
            $ast_use->lineno = self::getStartLine($use);
            // ast doesn't fill in an alias if it's identical to the real name,
            // but phpParser does?
            $name = ltrim(self::tokenToString($use->variableName), '$');
            $alias = $use->alias;
            $ast_use->children = [
                'name' => $name,
                'alias' => $alias !== $name ? $alias : null,
            ];
            $ast_uses[] = $ast_use;
        }
        return $ast_uses;
    }
     */

    /**
     * @return ast\Node
     */
    private static function astStmtUse($type, array $uses, int $line) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_USE;
        $node->flags = self::phpParserNamespaceUseKindToASTUseFlags($type);
        $node->lineno = $line;
        $node->children = $uses;
        return $node;
    }

    private static function astStmtGroupUse(int $type, ?string $prefix, array $uses, int $line) : ast\Node{
        $node = new ast\Node();
        $node->kind = ast\AST_GROUP_USE;
        $node->flags = self::phpParserNamespaceUseKindToASTUseFlags($type);
        $node->lineno = $line;
        $node->children = [
            'prefix' => $prefix,
            'uses' => new ast\Node(0, self::phpParserNamespaceUseKindToASTUseFlags($type), $uses, $line),
        ];
        return $node;
    }

    private static function astStmtEcho($expr, int $line) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_ECHO;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['expr' => $expr];
        return $node;
    }

    private static function astStmtReturn($expr, int $line) : ast\Node {
        $node = new ast\Node();
        $node->kind = ast\AST_RETURN;
        $node->flags = 0;
        $node->lineno = $line;
        $node->children = ['expr' => $expr];
        return $node;
    }

    private static function astIfElem($cond, $stmts, int $line) : ast\Node {
        return new ast\Node(ast\AST_IF_ELEM, 0, ['cond' => $cond, 'stmts' => $stmts], $line);
    }

    private static function phpParserSwitchListToAstSwitch(PhpParser\Node\Statement\SwitchStatementNode $node) {
        $stmts = [];
        $node_line = self::getEndLine($node) ?? 0;
        foreach ($node->caseStatements as $case) {
            $case_line = self::getEndLine($case);
            $stmts[] = new ast\Node(
                ast\AST_SWITCH_CASE,
                0,
                [
                    'cond' => $case->expression !== null ? self::phpParserNodeToAstNode($case->expression) : null,
                    'stmts' => self::phpParserStmtlistToAstNode($case->statementList, $case_line),
                ],
                $case_line ?? $node_line
            );
        }
        return new ast\Node(ast\AST_SWITCH, 0, [
            'cond' => self::phpParserNodeToAstNode($node->expression),
            'stmts' => new ast\Node(ast\AST_SWITCH_LIST, 0, $stmts, $stmts[0]->lineno ?? $node_line),
        ], $node_line);
    }

    private static function phpParserIfStmtToAstIfStmt(PhpParser\Node\Statement\IfStatementNode $node) : ast\Node {
        $start_line = self::getStartLine($node);
        $cond_line = self::getEndLine($node->expression) ?: $start_line;
        $if_elem = self::astIfElem(
            self::phpParserNodeToAstNode($node->expression),
            self::phpParserStmtlistToAstNode($node->statements, $cond_line),
            $start_line
        );
        $if_elems = [$if_elem];
        foreach ($node->elseIfClauses as $else_if) {
            $if_elem_line = self::getStartLine($else_if);
            $if_elem = self::astIfElem(
                self::phpParserNodeToAstNode($else_if->expression),
                self::phpParserStmtlistToAstNode($else_if->statements, $if_elem_line),
                $if_elem_line
            );
            $if_elems[] = $if_elem;
        }
        $parser_else_node = $node->elseClause;
        if ($parser_else_node) {
            $parser_else_line = self::getStartLine($parser_else_node);
            $if_elems[] = self::astIfElem(
                null,
                self::phpParserStmtlistToAstNode($parser_else_node->statements, $parser_else_line),
                $parser_else_line
            );
        }
        return new ast\Node(ast\AST_IF, 0, $if_elems, $start_line);

    }

    /**
     * @suppress PhanUndeclaredProperty
     */
    private static function astNodeAssignop(int $flags, PhpParser\Node $node, int $start_line) {
        return new ast\Node(
            ast\AST_ASSIGN_OP,
            $flags,
            [
                'var' => self::phpParserNodeToAstNode($node->var),
                'expr' => self::phpParserNodeToAstNode($node->expression),
            ],
            $start_line
        );
    }

    /**
     * @suppress PhanUndeclaredProperty
     */
    private static function astNodeBinaryop(int $flags, PhpParser\Node $n, int $start_line) {
        return new ast\Node(
            ast\AST_BINARY_OP,
            $flags,
            self::phpParserNodesToLeftRightChildren($n->left, $n->right),
            $start_line
        );
    }

    private static function phpParserNodesToLeftRightChildren($left, $right) : array {
        return [
            'left' => self::phpParserNodeToAstNode($left),
            'right' => self::phpParserNodeToAstNode($right),
        ];
    }

    private static function phpParserPropelemToAstPropelem(PhpParser\Node\Expression\AssignmentExpression $n, ?string $doc_comment) : ast\Node{
        $children = [
            'name' => self::phpParserNodeToAstNode($n->leftOperand),
            'default' => $n->rightOperand ? self::phpParserNodeToAstNode($n->rightOperand) : null,
        ];

        $start_line = self::getStartLine($n);

        return self::newAstNode(ast\AST_CONST_ELEM, 0, $children, $start_line, self::extractPhpdocComment($n) ?? $doc_comment);
    }

    private static function phpParserConstelemToAstConstelem(PhpParser\Node\ConstElement $n, ?string $doc_comment) : ast\Node{
        $children = [
            'name' => self::variableTokenToString($n->name),
            'value' => self::phpParserNodeToAstNode($n->assignment),
        ];

        $start_line = self::getStartLine($n);

        return self::newAstNode(ast\AST_CONST_ELEM, 0, $children, $start_line, self::extractPhpdocComment($n) ?? $doc_comment);
    }

    /**
     * @param PhpParser\Token[] $visibility
     */
    private static function phpParserVisibilityToAstVisibility(array $visibility, bool $automatically_add_public = true) : int {
        $ast_visibility = 0;
        foreach ($visibility as $token) {
            $part = \strtolower(self::tokenToString($token));
            switch($part) {
            case 'public':
                $ast_visibility |= ast\flags\MODIFIER_PUBLIC;
                break;
            case 'protected':
                $ast_visibility |= ast\flags\MODIFIER_PROTECTED;
                break;
            case 'private':
                $ast_visibility |= ast\flags\MODIFIER_PRIVATE;
                break;
            case 'static':
                $ast_visibility |= ast\flags\MODIFIER_STATIC;
                break;
            case 'abstract':
                $ast_visibility |= ast\flags\MODIFIER_ABSTRACT;
                break;
            case 'final':
                $ast_visibility |= ast\flags\MODIFIER_FINAL;
                break;
            default:
                throw new \RuntimeException("Unexpected visibility modifier '$part'");
            }
        }
        if ($automatically_add_public && !($ast_visibility & (ast\flags\MODIFIER_PUBLIC|ast\flags\MODIFIER_PROTECTED|ast\flags\MODIFIER_PRIVATE))) {
            $ast_visibility |= ast\flags\MODIFIER_PUBLIC;
        }
        return $ast_visibility;
    }

    // TODO: Don't need to pass in $start_line for many of these functions
    private static function phpParserPropertyToAstNode(PhpParser\Node\PropertyDeclaration $n, int $start_line) : ast\Node {
        $prop_elems = [];
        $doc_comment = $n->getDocCommentText();
        foreach ($n->propertyElements as $i => $prop) {
            $prop_elems[] = self::phpParserPropelemToAstPropelem($prop, $i === 0 ? $doc_comment : null);
        }
        $flags = self::phpParserVisibilityToAstVisibility($n->modifiers);

        return new ast\Node(ast\AST_PROP_DECL, $flags, $prop_elems, self::getStartLine($n) ?: $start_line);
    }

    private static function phpParserClassConstToAstNode(PhpParser\Node\ClassConstDeclaration $n, int $start_line) : ast\Node {
        $const_elems = [];
        $doc_comment = $n->getDocCommentText();
        foreach ($n->constElements as $i => $prop) {
            $const_elems[] = self::phpParserConstelemToAstConstelem($prop, $i === 0 ? $doc_comment : null);
        }
        $flags = self::phpParserVisibilityToAstVisibility($n->modifiers);

        return new ast\Node(ast\AST_CLASS_CONST_DECL, $flags, $const_elems, $const_elems[0]->lineno ?: $start_line);
    }

    private static function phpParserConstToAstNode(PhpParser\Node\Statement\ConstDeclaration $n, int $start_line) : ast\Node {
        $const_elems = [];
        $doc_comment = $n->getDocCommentText();
        foreach ($n->constElements as $i => $prop) {
            $const_elems[] = self::phpParserConstelemToAstConstelem($prop, $i === 0 ? $doc_comment : null);
        }

        return new ast\Node(ast\AST_CONST_DECL, 0, $const_elems, $const_elems[0]->lineno ?: $start_line);
    }

    /**
     * @suppress PhanUndeclaredProperty
     */
    private static function phpParserDeclareListToAstDeclares(PhpParser\Node\DeclareDirective $declares, int $start_line, string $first_doc_comment = null) : ast\Node {
        $ast_declare_elements = [];
        foreach ($declares as $declare) {
            $children = [
                'name' => self::tokenToString($declare->declareKeyword),
                'value' => self::tokenToString($declare->literal),
            ];
            $doc_comment = self::extractPhpdocComment($declare->getAttribute('comments')) ?? $first_doc_comment;
            $first_doc_comment = null;
            if (self::$ast_version >= 50) {
                $children['docComment'] = $doc_comment;
            }
            $node = new ast\Node(ast\AST_CONST_ELEM, 0, $children, $declare->getAttribute('startLine'));
            if (self::$ast_version < 50 && is_string($doc_comment)) {
                $node->docComment = $doc_comment;
            }
            $ast_declare_elements[] = $node;
        }
        return new ast\Node(ast\AST_CONST_DECL, 0, $ast_declare_elements, $start_line);

    }

    private static function astStmtDeclare(ast\Node $declares, ?ast\Node $stmts, int $start_line) : ast\Node{
        $children = [
            'declares' => $declares,
            'stmts' => $stmts,
        ];
        return new ast\Node(ast\AST_DECLARE, 0, $children, $start_line);
    }

    private static function astNodeCall($expr, $args, int $start_line) : ast\Node{
        if (\is_string($expr)) {
            if (substr($expr, 0, 1) === '\\') {
                $expr = substr($expr, 1);
            }
            $expr = new ast\Node(ast\AST_NAME, ast\flags\NAME_FQ, ['name' => $expr], $start_line);
        }
        return new ast\Node(ast\AST_CALL, 0, ['expr' => $expr, 'args' => $args], $start_line);
    }

    private static function astNodeMethodCall($expr, $method, ast\Node $args, int $start_line) : ast\Node {
        return new ast\Node(ast\AST_METHOD_CALL, 0, ['expr' => $expr, 'method' => $method, 'args' => $args], $start_line);
    }

    private static function astNodeStaticCall($class, $method, ast\Node $args, int $start_line) : ast\Node {
        // TODO: is this applicable?
        if (\is_string($class)) {
            if (substr($class, 0, 1) === '\\') {
                $expr = substr($class, 1);
            }
            $class = new ast\Node(ast\AST_NAME, ast\flags\NAME_FQ, ['name' => $class], $start_line);
        }
        return new ast\Node(ast\AST_STATIC_CALL, 0, ['class' => $class, 'method' => $method, 'args' => $args], $start_line);
    }

    // TODO: Get rid of this function?
    private static function extractPhpdocComment($comments) : ?string {
        if (\is_string($comments)) {
            return $comments;
        }
        if ($comments instanceof PhpParser\Node) {
            // TODO: Extract only the substring with doc comment text?
            return $comments->getDocCommentText() ?: null;
        }
        if ($comments === null) {
            return null;
        }
        assert(\is_array($comments));
        if (\count($comments) === 0) {
            return null;
        }

    }

    private static function phpParserListToAstList(PhpParser\Node\Expression\ArrayCreationExpression $n, int $start_line) : ast\Node {
        $ast_items = [];
        foreach ($n->arrayElements as $item) {
            if ($item === null) {
                $ast_items[] = null;
            } else {
                $ast_items[] = new ast\Node(ast\AST_ARRAY_ELEM, 0, [
                    'value' => self::phpParserNodeToAstNode($item->value),
                    'key' => $item->key !== null ? self::phpParserNodeToAstNode($item->key) : null,
                ], self::getStartLine($item));
            }
        }
        return new ast\Node(ast\AST_ARRAY, ast\flags\ARRAY_SYNTAX_LIST, $ast_items, $start_line);
    }

    private static function phpParserArrayToAstArray(PhpParser\Node\Expression\ArrayCreationExpression $n, int $start_line) : ast\Node {
        $ast_items = [];
        foreach ($n->arrayElements as $item) {
            if ($item === null) {
                $ast_items[] = null;
            } else {
                $flags = $item->byRef ? ast\flags\PARAM_REF : 0;
                $ast_items[] = new ast\Node(ast\AST_ARRAY_ELEM, $flags, [
                    'value' => self::phpParserNodeToAstNode($item->value),
                    'key' => $item->key !== null ? self::phpParserNodeToAstNode($item->key) : null,
                ], self::getStartLine($item));
            }
        }
        if (PHP_VERSION_ID < 70100) {
            $flags = 0;
        } else {
            $flags = ($n->openParenOrBracket->kind === TokenKind::OpenBracketToken ? ast\flags\ARRAY_SYNTAX_LONG : ast\flags\ARRAY_SYNTAX_SHORT);
        }
        return new ast\Node(ast\AST_ARRAY, $flags, $ast_items, $start_line);
    }

    /** @return ?ast\Node */
    private static function phpParserMemberAccessExpressionToAstProp(PhpParser\Node\Expression\MemberAccessExpression $n, int $start_line) {
        // TODO: Check for incomplete tokens?
        $name = self::phpParserNodeToAstNode($n->memberName);
        if ($name === null) {
            if (self::$should_add_placeholders) {
                $name = '__INCOMPLETE_PROPERTY__';
            } else {
                return null;
            }
        }
        return new ast\Node(ast\AST_PROP, 0, [
            'expr'  => self::phpParserNodeToAstNode($n->dereferencableExpression),
            'prop'  => $name,  // ast\Node|string
        ], $start_line);
    }

    /** @return int|string|float|bool|null */
    private static function tokenToScalar(PhpParser\Token $n) {
        $str = self::tokenToString($n);
        $int = \filter_var($str, FILTER_VALIDATE_INT);
        if ($int !== false) {
            return $int;
        }
        // TODO: other cases
        return $str;
    }

    private static function variableTokenToString(PhpParser\Token $n) : string {
        return ltrim(trim($n->getText(self::$file_contents)), '$');
    }

    private static function tokenToString(PhpParser\Token $n) : string {
        return trim($n->getText(self::$file_contents));
    }

    /**
     * @param PhpParser\Node\Expression|PhpParser\Node\QualifiedName|PhpParser\Token $scope_resolution_qualifier
     */
    private static function phpParserClassconstfetchToAstClassconstfetch($scope_resolution_qualifier, PhpParser\Token $name, int $start_line) : ?ast\Node {
        $name = self::tokenToString($name);
        // TODO: proper error handling of incomplete tokens?
        if ($name === null) {
            if (self::$should_add_placeholders) {
                $name = '__INCOMPLETE_CLASS_CONST__';
            } else {
                return null;
            }
        }
        return new ast\Node(ast\AST_CLASS_CONST, 0, [
            'class' => self::phpParserNodeToAstNode($scope_resolution_qualifier),
            'const' => $name,
        ], $start_line);
    }


    /**
     * @suppress PhanTypeMismatchProperty https://github.com/etsy/phan/issues/609
     * @suppress PhanUndeclaredProperty - docComment really exists.
     * NOTE: this may be removed in the future.
     *
     * Phan was used while developing this. The asserts can be cleaned up in the future.
     *
     * NOTE: in AST version <= 40, may creates docComment as a property, but in version >= 45, adds it to $children
     *
     * @return string
     */
    private static function phpParserNameToString(PhpParser\Node\QualifiedName $name) : string {
        // TODO: Handle error case
        return \implode('', \array_map(function(PhpParser\Token $token) : string {
            return trim(self::tokenToString($token));
        }, $name->parts));
    }

    const _NODES_WITH_NULL_DOC_COMMENT = [
        ast\AST_CONST_ELEM => true,
        ast\AST_PROP_ELEM => true,
    ];

    /**
     * @suppress PhanUndeclaredProperty - docComment really exists.
     * NOTE: this may be removed in the future.
     *
     * Phan was used while developing this. The asserts can be cleaned up in the future.
     *
     * NOTE: in AST version <= 40, may creates docComment as a property, but in version >= 45, adds it to $children
     *
     * @return ast\Node
     */
    private static function newAstNode(int $kind, int $flags, array $children, int $lineno, string $doc_comment = null) : ast\Node {
        if (self::$ast_version >= 50) {
            if (is_string($doc_comment) || array_key_exists($kind, self::_NODES_WITH_NULL_DOC_COMMENT)) {
                if ($kind !== ast\AST_CONST_ELEM || PHP_VERSION_ID >= 70100) {
                    $children['docComment'] = $doc_comment;
                }
            }
            return new ast\Node($kind, $flags, $children, $lineno);
        }
        $node = new ast\Node($kind, $flags, $children, $lineno);
        if (is_string($doc_comment)) {
            $node->docComment = $doc_comment;
        }
        return $node;
    }

    /**
     * @suppress PhanTypeMismatchProperty https://github.com/etsy/phan/issues/609
     * @suppress PhanUndeclaredProperty - docComment really exists.
     * NOTE: this may be removed in the future.
     *
     * Phan was used while developing this. The asserts can be cleaned up in the future.
     *
     * NOTE: in AST version >= 45, this returns Node, but in version <=40, this returns Decl
     *
     * @return ast\Node|ast\Node\Decl
     * @suppress PhanUndeclaredProperty
     */
    private static function newAstDecl(int $kind, int $flags, array $children, int $lineno, string $doc_comment = null, string $name = null, int $end_lineno = 0, int $decl_id = -1) : ast\Node {
        if (self::$ast_version >= 50) {
            $children50 = [];
            $children50['name'] = $name;
            $children50['docComment'] = $doc_comment;
            $children50 += $children;
            if ($decl_id >= 0 && self::$ast_version >= 50) {
                $children50['__declId'] = $decl_id;
            }
            $node = new ast\Node($kind, $flags, $children50, $lineno);
            if (is_int($end_lineno)) {
                $node->endLineno = $end_lineno;
            }
            return $node;
        }
        $decl = new ast\Node\Decl($kind, $flags, $children, $lineno);
        if (\is_string($doc_comment)) {
            $decl->docComment = $doc_comment;
        }
        $decl->name = $name;
        $decl->endLineno = $end_lineno;
        return $decl;
    }

    private static function nextDeclId() : int {
        return self::$decl_id++;
    }
}
