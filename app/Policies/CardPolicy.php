<?php

namespace App\Policies;

use App\Models\Card;
use App\Models\User;
use Illuminate\Auth\Access\HandlesAuthorization;

class CardPolicy
{
    use HandlesAuthorization;

    public function view(User $user, Card $card)
    {
        return $card->cardholder->id === $user->id;
    }

    public function update(User $user, Card $card)
    {
        return $card->cardholder->id === $user->id;
    }

    public function delete(User $user, Card $card)
    {
        return $card->cardholder->id === $user->id;
    }

}
