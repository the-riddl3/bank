@props(['disabled' => false])

<input {{ $disabled ? 'disabled' : '' }} {!! $attributes->merge(['class' => 'rounded-md shadow-sm border-orange-300 focus:border-orange-300 focus:ring focus:ring-orange-200 focus:ring-opacity-50']) !!}>
