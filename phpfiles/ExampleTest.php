<?php

class TestCase 
{
    // This is a test method
    // This method will be used to test if a condition is true
    /**
     * @param bool $condition
     * @return void
     */
    public function assertTrue(bool $condition): void
    {
        if ($condition === true) {
            echo "Test passed";
        } else {
            echo "Test failed";
        }
    }
}

class CalculatorTest extends TestCase
{
    // This is a test method 
    public function test_add(): void
    {
        $calculator = new Calculator();
        $this->assertTrue($calculator->add(5, 10) === 15);
    }
}

class Calculator {
    public function add(int $a, int $b): int {
        return $a + $b;
    }
}

$test = new CalculatorTest();

echo $test->test_add();