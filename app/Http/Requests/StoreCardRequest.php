<?php

namespace App\Http\Requests;

use App\Enums\CardTypeEnum;
use Illuminate\Foundation\Http\FormRequest;
use Illuminate\Validation\Rules\Enum;

class StoreCardRequest extends FormRequest
{


    /**
     * Get the validation rules that apply to the request.
     *
     * @return array<string, mixed>
     */
    public function rules()
    {
        return [
            'type' => [new Enum(CardTypeEnum::class)],
        ];
    }
}
