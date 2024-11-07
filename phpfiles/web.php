<?php

namespace App\Classes;

class Test {
    public function __construct() {
        echo "Test class has been initiated";
    }

    public function add(int $a, int $b): int {
        return $a + $b;
    }
}

$test = new Test();

echo $test->add(5, 10);
?>