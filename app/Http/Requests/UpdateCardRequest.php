<?php

namespace App\Http\Requests;

use App\Enums\CardTypeEnum;
use App\Models\Card;
use Illuminate\Foundation\Http\FormRequest;
use Illuminate\Validation\Rules\Enum;

class UpdateCardRequest extends FormRequest
{
    /**
     * Determine if the user is authorized to make this request.
     *
     * @return bool
     */
    public function authorize()
    {
        $card = Card::find($this->route('card'));
        return $this->user()->can('update', $card);
    }

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
