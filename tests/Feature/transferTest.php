<?php

namespace Tests\Feature;

use App\Models\Card;
use App\Models\User;
use Illuminate\Foundation\Testing\DatabaseMigrations;
use Illuminate\Foundation\Testing\RefreshDatabase;
use Illuminate\Foundation\Testing\WithFaker;
use Illuminate\Support\Facades\Hash;
use Illuminate\Support\Facades\Redis;
use Tests\TestCase;

class transferTest extends TestCase
{
    use DatabaseMigrations;

    private const TRANSACTION_NUMBER = 100;

    protected function tearDown(): void
    {
        // clear redis
        Redis::connection()->flushAll();
    }

    public function test_transfer()
    {
        // authenticate
        $user = User::create([
            'name' => 'riddle',
            'email' => 'riddle@test.com',
            'password' => Hash::make('admin'),
        ]);

        $this->actingAs($user);

        // get cards
        $card_sender = Card::create([
            'user_id' => $user->id,
            'type' => 0,
            'expiry' => now()->addYears(2),
        ]);

        $card_receiver = Card::create([
            'user_id' => $user->id,
            'type' => 0,
            'expiry' => now()->addYears(2),
        ]);

        // set balance
        $card_sender->balance = 10000;
        $card_sender->save();

        // make 100000 transfers and test time
        $start = microtime(true);
        for($i=0;$i<self::TRANSACTION_NUMBER;$i++) {
            $this->patch("/cards/$card_sender->id/transfer",[
                'receiver_card_id' => $card_receiver->id,
                'amount' => 0.1,
            ]);
        }
        $time = microtime(true) - $start;
        echo "optimized transfer time: " . $time . " for " . self::TRANSACTION_NUMBER . " transactions" . PHP_EOL;
    }

    public function test_transfer_slow()
    {
        // authenticate
        $user = User::create([
            'name' => 'riddle',
            'email' => 'riddle@test.com',
            'password' => Hash::make('admin'),
        ]);

        $this->actingAs($user);

        // get cards
        $card_sender = Card::create([
            'user_id' => $user->id,
            'type' => 0,
            'expiry' => now()->addYears(2),
        ]);

        $card_receiver = Card::create([
            'user_id' => $user->id,
            'type' => 0,
            'expiry' => now()->addYears(2),
        ]);

        // set balance
        $card_sender->balance = 10000;
        $card_sender->save();

        // test time
        $start = microtime(true);

        for($i=0;$i<self::TRANSACTION_NUMBER;$i++) {
            $this->patch("/cards/$card_sender->id/transferSlow",[
                'receiver_card_id' => $card_receiver->id,
                'amount' => 0.1,
            ]);
        }
        $time = microtime(true) - $start;
        echo "slow transfer time: " . $time . " for " . self::TRANSACTION_NUMBER . " transactions" . PHP_EOL;
    }
}
