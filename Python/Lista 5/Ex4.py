import copy

sudoku_field = [
    [0, 0, 0, 0, 8, 0, 0, 0, 0],
    [8, 0, 9, 0, 7, 1, 0, 2, 0],
    [4, 0, 3, 5, 0, 0, 0, 0, 1],
    [0, 0, 0, 1, 0, 0, 0, 0, 7],
    [0, 0, 2, 0, 3, 4, 0, 8, 0],
    [7, 3, 0, 0, 0, 9, 0, 0, 4],
    [9, 0, 0, 0, 0, 0, 7, 0, 2],
    [0, 0, 8, 2, 0, 5, 0, 9, 0],
    [1, 0, 0, 0, 4, 0, 3, 0, 0]
]


def sudoku_solver():
    size = len(sudoku_field)
    result_field = copy.deepcopy(sudoku_field)

    def can_insert(y, x, i):
        for m in range(size):
            if(result_field[y][m] == i or
               result_field[m][x] == i or
               result_field[y//3*3 + m % 3][x//3*3 + m//3] == i):
                return False
        return True

    def next(y, x):
        if x == size - 1 and y == size - 1:
            return True
        elif x != size - 1:
            return solve(y, x + 1)
        else:
            return solve(y + 1, 0)

    def solve(y, x):
        if (sudoku_field[y][x] == 0):
            for i in range(1, size + 1):  # 1 to 9
                if(can_insert(y, x, i)):
                    result_field[y][x] = i
                    if (next(y, x)):
                        return True
            result_field[y][x] = 0
            return False
        return next(y, x)

    def pretty_print():
        for i, line in enumerate(result_field):
            for j in range(size):
                if j != 0 and (j + 1) % ((size//3) + 1) == 0:
                    line.insert(j, '|')
            if i != 0 and i % (size // 3) == 0:
                print((3*(size + 2) + 4) * "-")
            print(line)
        return ""

    solve(0, 0)

    return pretty_print()


print(sudoku_solver())
