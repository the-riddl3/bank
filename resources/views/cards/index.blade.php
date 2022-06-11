<x-app-layout>
    <x-slot name="header">
        <h2 class="font-semibold text-xl text-gray-800 leading-tight">
            {{ __('Dashboard') }}
        </h2>
    </x-slot>

    <div class="py-12">
        <div class="max-w-7xl mx-auto sm:px-6 lg:px-8">
            <div class="bg-white overflow-hidden shadow-sm sm:rounded-lg grid grid-cols-4 gap-4">
                <div class="p-6 bg-white border-b border-gray-200">
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
            </div>
        </div>
    </div>
</x-app-layout>
