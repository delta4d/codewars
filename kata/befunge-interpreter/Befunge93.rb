class Befunge
    X = 80
    Y = 80
    def initialize(code)
        @map = Array.new(X) { ' ' * Y }
        code.lines.each_with_index do |r, i|
            r.chomp.chars.each_with_index { |c, j| @map[i][j] = c }
        end
        @stack = []
        @x = 0
        @y = 0
        @dir = :move_right
    end

    def _rand_dir
        %i[move_left move_right move_up move_down][rand(0..3)]
    end

    def op
        @map[@x][@y]
    end

    def push_num(c)
        @stack.push c.to_i
    end

    def add
        a, b = @stack.pop, @stack.pop
        @stack.push(a + b)
    end

    def min
        a, b = @stack.pop, @stack.pop
        @stack.push(b - a)
    end

    def mul
        a, b = @stack.pop, @stack.pop
        @stack.push(a * b)
    end

    def div
        a, b = @stack.pop, @stack.pop
        @stack.push(b / a)
    end

    def mod
        a, b = @stack.pop, @stack.pop
        @stack.push(a == 0 ? 0 : b / a)
    end

    def no
        a = @stack.pop
        @stack.push(a == 0 ? 1 : 0)
    end

    def gt
        a, b = @stack.pop, @stack.pop
        @stack.push(b > a ? 1 : 0)
    end

    def move
        send @dir
    end

    def move_right
        @y += 1
        @y = 0 if @y == Y
        @dir = :move_right
    end

    def move_left
        @y -= 1
        @y = Y - 1 if @y < 0
        @dir = :move_left
    end

    def move_up
        @x -= 1
        @x = X - 1 if @x < 0
        @dir = :move_up
    end

    def move_down
        @x += 1
        @x = 0 if @x == X
        @dir = :move_down
    end

    def move_random
        @dir = _rand_dir
        move
    end

    def pop_h
        a = @stack.pop
        @dir = a == 0 ? :move_right : :move_left
        move
    end

    def pop_v
        a = @stack.pop
        @dir = a == 0 ? :move_down : :move_up
        move
    end

    def scan_string
        move
        until op == '"'
            @stack.push op.ord
            move
        end
    end

    def dupl
        @stack.push (@stack.empty? ? 0 : @stack.last)
    end

    def swap
        a = @stack.pop
        b = @stack.empty? ? 0 : @stack.pop
        @stack.push a
        @stack.push b
    end

    def discard
        @stack.pop
    end

    def o_i
        @stack.pop.to_s
    end

    def o_a
        @stack.pop.chr
    end

    def skip
        move
    end

    def put
        x = @stack.pop
        y = @stack.pop
        v = @stack.pop
        @map[x][y] = v.chr
    end

    def get
        x = @stack.pop
        y = @stack.pop
        @stack.push @map[x][y].ord
    end

    def move_when_needed(c)
        move if "><v^?_|".count(c) == 0
    end

    def end?
        op == '@'
    end

    def to_s
        "(#{@x}, #{@y}) #{@dir} ===> #{op}, #{@stack}"
    end

    def run
        o = ""
        until end?
            
            # puts @map
            # puts self
            # puts o
            # puts '-'*79

            c = op
            case c
            when '0'..'9' then push_num c
            when '+'      then add
            when '-'      then min
            when '*'      then mul
            when '/'      then div
            when '%'      then mod
            when '!'      then no
            when '`'      then gt
            when '>'      then move_right
            when '<'      then move_left
            when '^'      then move_up
            when 'v'      then move_down
            when '?'      then move_random
            when '_'      then pop_h
            when '|'      then pop_v
            when '"'      then scan_string
            when ':'      then dupl
            when '\\'     then swap
            when '$'      then discard
            when '.'      then o += o_i;
            when ','      then o += o_a
            when '#'      then skip
            when 'p'      then put
            when 'g'      then get
            end
            move_when_needed c
        end
        o
    end
end

def interpret(code)
    Befunge.new(code).run
end

# File.open("in.txt", "r") do |f|
#     puts interpret(f.readlines.join).inspect
# end