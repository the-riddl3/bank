<?php

use App\Http\Controllers\CardController;
use Illuminate\Support\Facades\Route;

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/

Route::middleware('auth')->group(function() {
    Route::get('/', function () {
        return view('dashboard',[
            'cards' => auth()->user()->cards,
        ]);
    })->name('dashboard');

    // cards
    // cards
    Route::resource('cards', CardController::class);

    // card types
    Route::get('cards/types', static function() {
        return CardTypeResource::collection(CardTypeEnum::cases());
    });

    // extra card routes
    Route::prefix('cards/{card}/')->name('cards.')
        ->controller(CardController::class)
        ->group(function () {
            Route::patch('withdraw', 'withdraw')->name('withdraw');
            Route::patch('deposit', 'deposit')->name('deposit');
            Route::patch('transferSlow','transferSlow')->name('transferSlow');
        });
    Route::patch('cards/{card_id}/transfer', [CardController::class,'transfer'])->name('cards.transfer');
});

require __DIR__.'/auth.php';
