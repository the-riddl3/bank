<?php

namespace App\Http\Resources;

use Illuminate\Http\Resources\Json\JsonResource;

class CardResource extends JsonResource
{
    public function toArray($request)
    {
        return [
            'id' => $this->id,
            'type' => $this->type,
            'balance' => $this->balance,
            'cardholder' => $this->cardholder,
            'expiry' => $this->expiry,
        ];
    }
}
