<?php

namespace App\Providers;

use Illuminate\Support\ServiceProvider;
use Predis\Client;

class PredisServiceProvider extends ServiceProvider
{
    /**
     * Register services.
     *
     * @return void
     */
    public function register()
    {
        $this->app->singleton(Client::class, function($app) {
            return new Client(env('REDIS_HOST') ?? 'tcp://localhost:6379');
        });
    }

    /**
     * Bootstrap services.
     *
     * @return void
     */
    public function boot()
    {
        //
    }
}
