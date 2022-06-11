<x-app-layout>
    <x-slot name="header">
        <h2 class="font-semibold text-xl text-gray-800 leading-tight">
            {{ __('Dashboard') }}
        </h2>
    </x-slot>
    <div>
        <table class="table-auto">
            <thead>
            <tr>
                <th>card id</th>
                <th>balance</th>
            </tr>
            </thead>
            <tbody>
            @foreach($cards as $card)
                <tr class="p-4">
                    <td><a href="{{route('cards.show',$card->id)}}">{{$card->id}}</a></td>
                    <td>{{$card->balance}}</td>
                </tr>
            @endforeach
            </tbody>
        </table>
        <button>
            <a href="{{route('cards.create')}}">add a new card</a>
        </button>
    </div>
</x-app-layout>
