#!/usr/bin/env ruby

require "green_shoes"

Shoes.app do
  @dice = [:d4, :d6, :d8, :d10, :d12]
  list_box 
end