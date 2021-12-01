result() {
  sum=$(cat $1| sed 's/^\([^ ]*\) \([^ ]*\) bags contain .*/+(\1_\2() ? 1 : 0)\\/')
script=$(cat $1| grep -v '^shiny gold' | grep -v 'contain no other' | sed 's/^\([^ ]*\) \([^ ]*\) bags contain/def \1_\2();/' | sed 's/. \([^ ]*\) \([^ ]*\) bags\?/\1_\2() /g' | sed 's/,/||/g' | sed 's/\./; end;/')
echo "
def method_missing(m, *args, &block)
  false
end
def shiny_gold()
  true
end
$script
p (0 $sum
- 1)
" > /tmp/script.rb
ruby /tmp/script.rb
}

# result 7_example_input
result 7_input
