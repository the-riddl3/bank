<?php

namespace App\Models;

use Brick\Math\BigInteger;
use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;

class Transaction
{
    public int $sender_card_id;
    public int $receiver_card_id;
    public float $amount;

    /**
     * @param int $sender_card_id
     * @param int $receiver_card_id
     * @param float $amount
     */
    public function __construct(int $sender_card_id, int $receiver_card_id, float $amount)
    {
        $this->sender_card_id = $sender_card_id;
        $this->receiver_card_id = $receiver_card_id;
        $this->amount = $amount;
    }
}
