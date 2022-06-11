<?php

namespace App\Enums;

enum CardTypeEnum: int
{
    case Regular = 0;
    case Plus = 1;
    case Gold = 2;

    public function name(): string {
        return match($this) {
            self::Regular => 'Regular',
            self::Plus => 'Plus',
            self::Gold => 'Gold',
        };
    }
}
