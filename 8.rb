program = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6".split("\n")

program = File.read("8").split("\n")

# part 1

# part 2

jmp_lines = program.each_with_index.select { |line, i| line.start_with? "jmp" }.map { |line, i| i }
done = false

jmp_lines.each do  |jmp_line|
  acc=0
  i=0
  visited=[]
  while not visited.include?(i)
    if  i >= program.size
      done = true
      break
    end
    visited.push(i)
    line = (i == jmp_line ? "nop +0" : program[i]).split(" ")
    line = [line[0].to_sym, line[1].to_i]
    if line[0] == :acc
      then
      acc += line[1]
      i += 1
    elsif line[0] == :jmp
      i += line[1]
    else
      i += 1
    end

  end
  p acc
  break if done
end
