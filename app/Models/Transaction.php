<?php

namespace App\Models;

use App\Enums\TransactionStatusEnum\TransactionStatusEnum;
use Illuminate\Database\Eloquent\Factories\HasFactory;
use Illuminate\Database\Eloquent\Model;

class Transaction extends Model
{
    use HasFactory;

    protected $fillable = ['receiver_card_id','sender_card_id','amount','status'];
    protected $casts = [
        'status' => TransactionStatusEnum::class,
    ];

    protected $hidden = ['status','created_at','updated_at'];
}
