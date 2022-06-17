<x-app-layout>
    <x-slot name="header">
        <h2 class="font-semibold text-xl text-gray-800 leading-tight">
            {{ __('Dashboard') }}
        </h2>
    </x-slot>

    <div class="p-4 w-1/3 flex justify-center items-center bg-white overflow-hidden shadow-sm sm:rounded-lg">
        <form class="flex flex-col w-1/3 justify-center items-center" action="{{route('cards.store')}}" method="post">
            @csrf
            <label>
                Card Type
                <select name="type">
                    @foreach($types as $type)
                        <option value="{{$type->value}}">{{$type->name}}</option>
                    @endforeach
                </select>
            </label>
            <x-button class="my-2">create</x-button>
        </form>
    </div>
</x-app-layout>
