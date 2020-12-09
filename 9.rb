nums = %w(
  35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
)

nums = File.read("9").split("\n")
nums = nums.map { |x| x.to_i }

# part 1

n = 25
last_n = []

anomaly = 0

nums.each_with_index do |num, index|
  if index < n
    last_n.push num
  else
    contains_sum = false
    last_n.each do |old_num|
      other_num = num - old_num
      if last_n.include? other_num
        contains_sum = true
        break
      end
    end
    if !contains_sum
      anomaly = num
      p num
      break
    end
    last_n.shift
    last_n.push num
  end
end

# part 2

(0..nums.size).each do |start|
  break if nums[start] > anomaly
  (start..nums.size).each do |fin|
    break if nums[fin] > anomaly
    list = nums[start..fin]
    if anomaly == list.reduce(:+)
      p list
      p list.min + list.max
      exit
    end
  end
end
