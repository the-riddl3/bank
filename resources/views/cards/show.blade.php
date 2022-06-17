<x-app-layout>
    <div class="flex w-full items-center p-4">
        <div
            class="overflow-hidden mx-3 flex justify-around border-4 border-gray-300 bg-white shadow-sm sm:rounded-lg w-1/3">
            <div class="p-6">
                <img src="{{$card->image}}" class="object-contain" alt="card image">
                <p>id - {{$card->id}}</p>
                <p>type - <span @class(['text-center',
                                'card-regular' => $card->type->name === 'Regular',
                                'card-gold' => $card->type->name === 'Gold'
                               ])
                    >{{$card->type->name}}</span></p>
                <p>balance - <span class="text-green-600">{{$card->balance}}</span>$</p>
            </div>

            <div class="flex flex-col items-center justify-center w-full gap-4 my-2 p-2">
                <x-button
                    type="button" data-modal-toggle="depositModal">
                    deposit
                </x-button>

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
                                <x-button type="button"
                                          data-modal-toggle="depositModal">
                                    <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                         xmlns="http://www.w3.org/2000/svg">
                                        <path fill-rule="evenodd"
                                              d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                              clip-rule="evenodd"></path>
                                    </svg>
                                </x-button>
                            </div>
                            <form action="{{route('cards.deposit',$card->id)}}" method="post">
                            @method('PATCH')
                            @csrf
                            <!-- Modal body -->
                                <div class="p-6 space-y-6">
                                    <input class="input-primary" type="number" name="amount" placeholder="amount"
                                           min="0"/>
                                </div>
                                <!-- Modal footer -->
                                <div
                                    class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                    <x-button data-modal-toggle="depositModal"
                                    >
                                        ok
                                    </x-button>
                                    <x-button data-modal-toggle="depositModal" type="button">
                                        Cancel
                                    </x-button>
                                </div>
                            </form>
                        </div>
                    </div>
                </div>

                <!-- Modal toggle -->
                <x-button class="btn-primary"
                          type="button" data-modal-toggle="withdrawModal">
                    withdraw
                </x-button>

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
                                <x-button type="button"
                                          data-modal-toggle="withdrawModal">
                                    <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                         xmlns="http://www.w3.org/2000/svg">
                                        <path fill-rule="evenodd"
                                              d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                              clip-rule="evenodd"></path>
                                    </svg>
                                </x-button>
                            </div>

                            <form action="{{route('cards.withdraw',$card->id)}}" method="post">
                            @method('PATCH')
                            @csrf
                            <!-- Modal body -->
                                <div class="p-6 space-y-6">
                                    <input class="input-primary" type="number" name="amount" placeholder="amount"
                                           min="0">
                                </div>
                                <!-- Modal footer -->
                                <div
                                    class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                    <x-button data-modal-toggle="withdrawModal"
                                    >
                                        ok
                                    </x-button>
                                    <x-button data-modal-toggle="withdrawModal" type="button"
                                    >
                                        Cancel
                                    </x-button>
                                </div>
                            </form>
                        </div>
                    </div>
                </div>

                <!-- Modal toggle -->
                <x-button class="btn-primary"
                          type="button" data-modal-toggle="transferModal">
                    transfer
                </x-button>

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
                                <x-button type="button"
                                          data-modal-toggle="transferModal">
                                    <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20"
                                         xmlns="http://www.w3.org/2000/svg">
                                        <path fill-rule="evenodd"
                                              d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                              clip-rule="evenodd"></path>
                                    </svg>
                                </x-button>
                            </div>

                            <form action="{{route('cards.transfer',$card->id)}}" method="post">
                            @csrf
                            @method('PATCH')
                            <!-- Modal body -->
                                <div class="p-6 space-y-6 flex flex-col items-center justify-center">
                                    <input class="input-primary" type="number" name="amount" placeholder="amount"
                                           min="0">
                                    <div>
                                        <input name="receiver_card_id" x-model="receiverId"
                                               @input="receiverName = (await getCard(receiverId)).cardholder.name"
                                               class="input-primary" type="text" placeholder="receiver card"
                                               id="receiverCard">
                                        <p x-text="receiverName"></p>
                                    </div>
                                </div>
                                <!-- Modal footer -->
                                <div
                                    class="flex items-center p-6 space-x-2 rounded-b border-t border-gray-200 dark:border-gray-600">
                                    <x-button data-modal-toggle="transferModal">
                                        ok
                                    </x-button>
                                    <x-button data-modal-toggle="transferModal" type="button"
                                    >
                                        Cancel
                                    </x-button>
                                </div>
                            </form>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <!-- transactions -->
        <div class="border-4 border-gray-300 flex-grow p-4 relative overflow-x-auto shadow-md sm:rounded-lg">
            <table class="w-full text-sm text-left text-gray-500 dark:text-gray-400">
                <thead class="text-xs text-gray-700 uppercase bg-gray-50 dark:bg-gray-700 dark:text-gray-400">
                <tr>
                    <th scope="col" class="px-6 py-3">
                        sent/received
                    </th>
                    <th scope="col" class="px-6 py-3">
                        sender/receiver
                    </th>
                    <th scope="col" class="px-6 py-3">
                        amount
                    </th>
                    <th scope="col" class="px-6 py-3">
                        status
                    </th>
                </tr>
                </thead>
                <tbody class="overflow-y-auto">
                @if(count($card->transactions()->get()) !== 0)
                    @foreach($card->transactions()->get() as $transaction)
                        <tr class="bg-white border-b dark:bg-gray-800 dark:border-gray-700">
                            @if($transaction->sender_card_id === $card->id)
                                <td class="px-6 py-4">-></td>
                                <td scope="row"
                                    class="px-6 py-4 font-medium text-gray-900 dark:text-white whitespace-nowrap">{{$transaction->receiver_card_id}}</td>
                            @else
                                <td class="px-6 py-4"><-</td>
                                <td scope="row"
                                    class="px-6 py-4 font-medium text-gray-900 dark:text-white whitespace-nowrap">{{$transaction->sender_card_id}}</td>
                            @endif
                            <td>{{$transaction->amount}}</td>
                            <td @class([
                                        'text-gray-400' => $transaction->status->name() === 'Pending',
                                        'text-green-500' => $transaction->status->name() === 'Complete',
                                        'text-red-500' => $transaction->status->name() === 'Failed',
                                        ])>{{$transaction->status->name()}}</td>
                        </tr>
                    @endforeach
                @else
                    <tr>
                        <td colspan="4" class="text-center px-6 py-4">No transactions</td>
                    </tr>
                @endif
                </tbody>
            </table>
        </div>
    </div>
    @push('scripts')
        <script>
            const getCard = async id => (await axios.get(`/api/cards/${id}`)).data;
        </script>
    @endpush
</x-app-layout>
