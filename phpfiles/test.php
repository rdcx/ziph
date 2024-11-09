<?php
$i = 10;
$sum = 0;

function add() {
    if ($i != 10) {
        $sum = $sum + 1;
    } else {
        add();
    }

    return $sum;
}

return add();