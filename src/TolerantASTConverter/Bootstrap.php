<?php declare(strict_types=1);

// Listen for all errors
error_reporting(E_ALL);

// Add the root to the include path
define('CLASS_DIR', __DIR__ . '/../');
set_include_path(get_include_path() . PATH_SEPARATOR . CLASS_DIR);

// Use the composer autoloader
foreach ([
    __DIR__ . '/../../vendor/autoload.php',          // autoloader is in this project
    __DIR__ . '/../../../../../vendor/autoload.php', // autoloader is in parent project
    ] as $file) {
    if (file_exists($file)) {
        echo "Found the autoloader at $file\n";
        require_once($file);
        break;
    }
}
