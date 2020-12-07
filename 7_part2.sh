result() {
  script=$(cat $1| grep -v 'contain no other' | sed 's/^\([^ ]*\) \([^ ]*\) bags contain/def \1_\2(); 1 + /' | sed 's/\(.\) \([^ ]*\) \([^ ]*\) bags\?/\1 * \2_\3() /g' | sed 's/,/+/g' | sed 's/\./; end;/')
echo "
def method_missing(m, *args, &block)
  1
end
$script
p shiny_gold - 1
" > /tmp/script.rb
ruby /tmp/script.rb
}

#result 7_example_input_2
result 7_input
