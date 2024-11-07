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

class ExampleTest extends TestCase
{
    // This is a test method 
    public function test_that_true_is_true(): void
    {
        $this->assertTrue(true);
    }
}

$test = new ExampleTest();

echo $test->test_that_true_is_true();