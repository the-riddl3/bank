<?php

namespace App\Enums\TransactionStatusEnum;

enum TransactionStatusEnum:int
{
    case PENDING = 0;
    case COMPLETE = 1;
    case FAILED = 2;

    public function name(): string {
        return match($this) {
            self::PENDING => 'Pending',
            self::COMPLETE => 'Complete',
            self::FAILED => 'Failed',
        };
    }
}
