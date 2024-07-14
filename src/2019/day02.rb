class IntCode
  attr_reader :memory, :debug, :boot, :ip

  def initialize(memory, boot: nil, debug: false)
    @memory = memory.clone
    @debug = debug
    @boot = boot
    @ip = 0
  end

  def init_memory
    @memory[1] = boot[0]
    @memory[2] = boot[1]
  end

  def run
    init_memory if boot
    loop do
      step
      break if op_name == :halt
      @ip += 4
    end
    memory[0]
  end

  def step
    trace if debug
    self.send(op_name)
  end

  def op_name
    { 1 => :add, 2 => :mul, 99 => :halt }[op]
  end

  def trace
    puts "running #{op_name} @ #{ip}: #{memory[ip..ip+3]}"
  end

  def op
    memory[ip]
  end

  def x
    memory[ip + 1]
  end

  def y
    memory[ip + 2]
  end

  def dest
    memory[ip + 3]
  end

  def add
    @memory[dest] = memory[x] + memory[y]
  end

  def mul
    @memory[dest] = memory[x] * memory[y]
  end

  def halt
    memory[0]
  end
end

## Part 1
test = "1,9,10,3,2,3,11,0,99,30,40,50"
input = File.read("day02.dat")
data = input.split(",").map(&:to_i)
comp = IntCode.new(data, boot: [12, 02], debug: true)
puts comp.run

## Part 2
answer = 19690720 # the date of the moon landing, cute
(1..99).each do |x|
  (1..99).each do |y|
    comp = IntCode.new(data, boot: [x, y])
    result = comp.run
    puts 100 * x + y
    exit if result == answer
  end
end
