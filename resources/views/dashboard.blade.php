<x-app-layout>
    <div class="w-full flex justify-end">
        <a href="{{route('cards.create')}}">
            <x-button>
                add a new card
            </x-button>
        </a>
    </div>
    <div class="flex items-center my-2">
        @foreach($cards as $card)
            <div
                class="flex mx-2 flex-col items-center justify-center p-3 w-1/5 border-2 border-gray-200 rounded-xl hover:scale-105 hover:bg-gray-200">
                <a href="{{route('cards.show',$card->id)}}">
                    <h2 @class(['text-center',
                                'card-regular' => $card->type->name === 'Regular',
                                'card-gold' => $card->type->name === 'Gold'
                               ])>{{$card->type->name}}</h2>
                    <img src="{{$card->image}}" alt="card image" class="object-contain">
                    <h2 class="text-center">{{$card->id}}</h2>
                </a>
            </div>
        @endforeach
    </div>
</x-app-layout>
