<?php

use Carbon\Carbon;
use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\DB;
use Illuminate\Support\Facades\Schema;

return new class extends Migration
{
    /**
     * Run the migrations.
     *
     * @return void
     */
    public function up()
    {
        Schema::create('cards', function (Blueprint $table) {
            // 12 digit id
            $table->id()->from(100000000000);
            // cardholder
            $table->foreignId('user_id')
                ->references('id')->on('users')->onDelete('cascade');
            // type
            $table->tinyInteger('type');
            // balance
            $table->decimal('balance')->default(100.00);
            // expiration date - by default - 2 years from now
            $table->date('expiry');
            $table->timestamps();
        });
    }

    /**
     * Reverse the migrations.
     *
     * @return void
     */
    public function down()
    {
        Schema::dropIfExists('cards');
    }
};
