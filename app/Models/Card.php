<?php

namespace App\Models;

use App\Enums\CardTypeEnum;
use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;
use Illuminate\Database\Eloquent\Relations\BelongsTo;

class Card extends Model
{
    use HasFactory;

    protected $fillable = ['type','user_id','expiry'];

    protected $casts = [
        'type' => CardTypeEnum::class,
    ];

    public function cardholder(): BelongsTo
    {
        return $this->belongsTo(User::class,'user_id','id');
    }

    public function withdraw(int $amount): bool
    {
        // if not enough balance
        if($amount > $this->balance) {
            return false;
        }

        // withdraw
        $this->balance -= $amount;

        // persist
        $this->save();

        return true;
    }

    public function deposit(int $amount): bool
    {
        // deposit
        $this->balance += $amount;

        // persist
        $this->save();

        return true;
    }

    public function transfer(Card $receiver_card,int $amount): bool
    {
        // if not enough balance
        if($amount > $this->balance) {
            return false;
        }

        // transfer
        $this->balance -= $amount;
        $receiver_card->balance += $amount;

        // persist
        $this->save();
        $receiver_card->save();

        return true;
    }
}
