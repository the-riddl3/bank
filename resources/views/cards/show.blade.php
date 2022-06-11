<x-app-layout>
    <x-slot name="header">
        <h2 class="font-semibold text-xl text-gray-800 leading-tight">
            {{ __('Dashboard') }}
        </h2>
    </x-slot>

    <div class="py-12 flex flex-col bg-white rounded-md items-center">
        <div class="max-w-7xl mx-auto sm:px-6 lg:px-8">
            <div class="bg-white overflow-hidden shadow-sm sm:rounded-lg">
                <div class="p-6 bg-white border-b border-gray-200">
                    <p>id - {{$card->id}}</p>
                    <p>type - {{$card->type->name}}</p>
                    <p>balance - <span class="text-green-600">{{$card->balance}}</span>$</p>
                </div>
            </div>
        </div>

        <!-- button class="btn-primary"s -->
        <!-- deposit -->
        <div class="grid grid-cols-4 w-full gap-4 border">
            <button class="btn-primary"
                    type="button" data-modal-toggle="depositModal">
                deposit
            </button>

            <!-- deposit modal -->
            <div id="depositModal" tabindex="-1" aria-hidden="true"
                 class="hidden overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 w-full md:inset-0 h-modal md:h-full">
                <div class="relative p-4 w-full max-w-2xl h-full md:h-auto">
                    <!-- Modal content -->
                    <div class="relative bg-white rounded-lg shadow dark:bg-gray-700">
                        <!-- Modal header -->
                        <div class="flex justify-between items-start p-4 rounded-t border-b dark:border-gray-600">
                            <h3 class="text-xl font-semibold text-gray-900 dark:text-white">
                                deposit
                            </h3>
                            <button type="button" class="btn-primary"
                                    data-modal-toggle="depositModal">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                     xmlns="http://www.w3.org/2000/svg">
                                    <path fill-rule="evenodd"
                                          d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                          clip-rule="evenodd"></path>
                                </svg>
                            </button class="btn-primary">
                        </div>
                        <form action="{{route('cards.deposit',$card->id)}}" method="post">
                        @method('PATCH')
                        @csrf
                        <!-- Modal body -->
                            <div class="p-6 space-y-6">
                                <input class="input-primary" type="number" name="amount" placeholder="amount" min="0"/>
                            </div>
                            <!-- Modal footer -->
                            <div
                                class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                <button class="btn-primary" data-modal-toggle="depositModal"
                                >
                                    ok
                                </button>
                                <button class="btn-primary" data-modal-toggle="depositModal" type="button">
                                    Cancel
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            </div>

            <!-- Modal toggle -->
            <button class="btn-primary"
                    type="button" data-modal-toggle="withdrawModal">
                withdraw
            </button>

            <!-- withdraw modal -->
            <div id="withdrawModal" tabindex="-1" aria-hidden="true"
                 class="hidden overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 w-full md:inset-0 h-modal md:h-full">
                <div class="relative p-4 w-full max-w-2xl h-full md:h-auto">
                    <!-- Modal content -->
                    <div class="relative bg-white rounded-lg shadow dark:bg-gray-700">
                        <!-- Modal header -->
                        <div class="flex justify-between items-start p-4 rounded-t border-b dark:border-gray-600">
                            <h3 class="text-xl font-semibold text-gray-900 dark:text-white">
                                withdraw
                            </h3>
                            <button class="btn-primary" type="button"
                                    data-modal-toggle="withdrawModal">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                     xmlns="http://www.w3.org/2000/svg">
                                    <path fill-rule="evenodd"
                                          d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                          clip-rule="evenodd"></path>
                                </svg>
                            </button>
                        </div>

                        <form action="{{route('cards.withdraw',$card->id)}}" method="post">
                        @method('PATCH')
                        @csrf
                        <!-- Modal body -->
                            <div class="p-6 space-y-6">
                                <input class="input-primary" type="number" name="amount" placeholder="amount" min="0">
                            </div>
                            <!-- Modal footer -->
                            <div
                                class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                <button class="btn-primary" data-modal-toggle="withdrawModal"
                                >
                                    ok
                                </button>
                                <button class="btn-primary" data-modal-toggle="withdrawModal" type="button"
                                >
                                    Cancel
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            </div>

            <!-- Modal toggle -->
            <button class="btn-primary"
                    type="button" data-modal-toggle="transferModal">
                transfer
            </button>

            <!-- transfer modal -->
            <div id="transferModal" tabindex="-1" aria-hidden="true" x-data="{receiverId: '',receiverName: ''}"
                 class="hidden overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 w-full md:inset-0 h-modal md:h-full">
                <div class="relative p-4 w-full max-w-2xl h-full md:h-auto">
                    <!-- Modal content -->
                    <div class="relative bg-white rounded-lg shadow dark:bg-gray-700">
                        <!-- Modal header -->
                        <div class="flex justify-between items-start p-4 rounded-t border-b dark:border-gray-600">
                            <h3 class="text-xl font-semibold text-gray-900 dark:text-white">
                                transfer
                            </h3>
                            <button class="btn-primary" type="button"
                                    data-modal-toggle="transferModal">
                                <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                     xmlns="http://www.w3.org/2000/svg">
                                    <path fill-rule="evenodd"
                                          d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                          clip-rule="evenodd"></path>
                                </svg>
                            </button>
                        </div>

                        <form action="{{route('cards.transfer',$card->id)}}" method="post">
                        @csrf
                        @method('PATCH')
                        <!-- Modal body -->
                            <div class="p-6 space-y-6 flex flex-col items-center justify-center">
                                <input class="input-primary" type="number" name="amount" placeholder="amount" min="0">
                                <div>
                                    <input name="receiver_card_id" x-model="receiverId" @input.debounce="receiverName = (await getCard(receiverId)).cardholder.name" class="input-primary" type="text" placeholder="receiver card" id="receiverCard">
                                    <p x-text="receiverName"></p>
                                </div>
                            </div>
                            <!-- Modal footer -->
                            <div
                                class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                <button class="btn-primary" data-modal-toggle="transferModal">
                                    ok
                                </button>
                                <button class="btn-primary" data-modal-toggle="transferModal" type="button"
                                >
                                    Cancel
                                </button>
                            </div>
                        </form>
                    </div>
                </div>
            </div>
        </div>
    </div>
    @push('scripts')
        <script>
            const getCard = async id => (await axios.get(`/api/cards/${id}`)).data;
        </script>
    @endpush
</x-app-layout>
