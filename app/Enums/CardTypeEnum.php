<?php

namespace App\Enums;

enum CardTypeEnum: int
{
    case Regular = 0;
    case Gold = 2;

    public function name(): string {
        return match($this) {
            self::Regular => 'Regular',
            self::Gold => 'Gold',
        };
    }
}
