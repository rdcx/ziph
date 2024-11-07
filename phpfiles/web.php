<?php

namespace App\Classes;

class Web {
    public static function init() {
        echo "Web class has been initiated";
    }

    public function __construct() {
        echo "Web class has been initiated";
    }
}

Web::init();

?>