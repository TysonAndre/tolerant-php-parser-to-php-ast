<?php declare(strict_types=1);

namespace TolerantASTConverter;

use Exception;

/**
 * @internal thrown when processing something that would become an invalid variable.
 */
final class InvalidNodeException extends Exception
{
}
