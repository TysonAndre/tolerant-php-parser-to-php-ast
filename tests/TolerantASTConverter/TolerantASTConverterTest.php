<?php

declare(strict_types=1);

namespace TolerantASTConverter\Tests;

use TolerantASTConverter\TolerantASTConverter;

/**
 * Tests that the polyfill works with valid ASTs
 */
class TolerantASTConverterTest extends \PHPUnit\Framework\TestCase
{
    public function testGenerateCacheKey(): void
    {
        $this->assertSame(
            (new TolerantASTConverter())->generateCacheKey('Some file contents', 70),
            (new TolerantASTConverter())->generateCacheKey('Some file contents', 70),
            'generateCacheKey should be idempotent'
        );
    }
}
