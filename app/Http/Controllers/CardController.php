<?php

namespace App\Http\Controllers;

use App\Enums\CardTypeEnum;
use App\Enums\TransactionStatusEnum\TransactionStatusEnum;
use App\Http\Requests\StoreCardRequest;
use App\Http\Requests\UpdateCardRequest;
use App\Http\Resources\CardResource;
use App\Http\Resources\CardTypeResource;
use App\Models\Card;
use App\Models\Transaction;
use App\Models\User;
use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Redis;
use Predis\Client;

class CardController extends Controller
{
    public function index()
    {
        return view('cards.index', [
            'cards' => auth()->user()->cards,
        ]);
    }

    public function create()
    {
        return view('cards.create', [
            'types' => CardTypeResource::collection(CardTypeEnum::cases()),
        ]);
    }

    public function store(StoreCardRequest $request)
    {
        $card = Card::create([
            'user_id' => auth()->id(),
            'type' => $request->get('type'),
            // expires in 2 years from now
            'expiry' => now()->addYears(2),
        ]);

        return redirect('/');
    }

    public function show(Card $card)
    {
        return view('cards.show', [
            'card' => new CardResource($card),
        ]);
    }

    public function edit(Card $card)
    {
        return view('cards.edit', [
            'card' => new CardResource($card),
        ]);
    }

    public function update(UpdateCardRequest $request, Card $card)
    {
        $card->fill($request->all());
        $card->save();

        return redirect('/');
    }

    public function destroy(Card $card)
    {
        // authorize if user can delete card
        $this->authorize('delete', $card);

        $card->delete();

        return redirect('/');
    }

    public function withdraw(Request $request, Card $card)
    {
        // validation
        $validated = $request->validate([
            'amount' => 'required|min:0'
        ]);

        // authorize
        $this->authorize('update', $card);

        // withdraw
        $success = $card->withdraw($validated['amount']);

        if (!$success) {
            return redirect()->back()->withErrors([
                'message' => 'not enough balance'
            ]);
        }

        return redirect()->back();
    }

    public function deposit(Request $request, Card $card)
    {
        // validation
        $validated = $request->validate([
            'amount' => 'required|min:0'
        ]);

        // authorize
        $this->authorize('update', $card);

        // deposit
        $success = $card->deposit($validated['amount']);

        if (!$success) {
            return redirect()->back()->withErrors([
                'message' => 'not enough balance'
            ]);
        }

        return redirect()->back();
    }

    public function transfer(Request $request, int $card_id)
    {
        // validation
        $validated = $request->validate([
            'receiver_card_id' => 'required|exists:cards,id',
            'amount' => 'required|min:0',
        ]);

        // create a pending transaction
        $transaction = Transaction::create([
            'sender_card_id' => $card_id,
            'receiver_card_id' => (int)$validated['receiver_card_id'],
            'amount' => (float)$validated['amount'],
            'status' => TransactionStatusEnum::PENDING,
        ]);

        // rpush serialized transaction to redis queue
        Redis::rPush('list:transactions', $transaction->toJson());

        return redirect()->back();
    }

    public function transferSlow(Request $request, Card $card)
    {
        // validation
        $validated = $request->validate([
            'receiver_card_id' => 'required|exists:cards,id',
            'amount' => 'required|min:0',
        ]);

        // authorize
        $this->authorize('update', $card);

        // transfer
        $success = $card->transfer(Card::find(
            $validated['receiver_card_id']),
            $validated['amount']
        );

        if (!$success) {
            // create a failed transaction
            $transaction = Transaction::create([
                'sender_card_id' => $card_id,
                'receiver_card_id' => (int)$validated['receiver_card_id'],
                'amount' => (float)$validated['amount'],
                'status' => TransactionStatusEnum::FAILED,
            ]);
            return redirect()->back()->withErrors([
                'message' => 'not enough balance'
            ]);
        }

        // create a complete transaction
        $transaction = Transaction::create([
            'sender_card_id' => $card_id,
            'receiver_card_id' => (int)$validated['receiver_card_id'],
            'amount' => (float)$validated['amount'],
            'status' => TransactionStatusEnum::COMPLETE,
        ]);

        return redirect()->back();
    }

    public function getCard(Card $card): JsonResponse
    {
        return response()->json(new CardResource($card));
    }
}
