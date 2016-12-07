#!/usr/bin/env ruby
# ruby is secretly perl, right?

mode = ARGV.member?('--initial') ? :sum : :decrypt
output = 0
$stdin.readlines.each do |s|
  (_,cs,id,key) = /(.*?)(\d+)\[([a-z]{5})\]/.match(s).to_a
  h = Hash.new {|h,k| h[k] = 0}
  cs.each_char { |c| h[c] = 1+h[c] unless c == ?- }
  hchars = h.sort.sort {|x,y|y[1]<=>x[1]}.take(5).map(&:first).join
  is_real = key == hchars
  id = id.to_i
  if :decrypt == mode and is_real then
    alf = 'abcdefghijklmnopqrstuvwxyz'
    name = cs.tr(?-, ' ').tr(alf, alf[id%26,26]+alf[0,id%26])
    is_real = /north.*pole/.match(name)
  end
  output += id if is_real
end
puts output
