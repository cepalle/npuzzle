import sys
from variables import MOVE_UP, MOVE_DOWN, MOVE_LEFT, MOVE_RIGHT, EMPTY_CASE
from utils import add_piece_direction, reverse_move
from visu import PuzzleWindow


def find_empty_case(grid, grid_size):
    for y in range(grid_size):
        for x in range(grid_size):
            if grid[y][x] == EMPTY_CASE:
                return x, y
    return -1, -1


def read_move(line):
    if line == "Down":
        return MOVE_UP
    elif line == "Up":
        return MOVE_DOWN
    elif line == "Right":
        return MOVE_LEFT
    elif line == "Left":
        return MOVE_RIGHT


def return_splitted(line):
    splitted = line.split(" = ")
    if len(splitted) != 2:
        return ""
    return int(splitted[1])

def read_input(speed_lvl=0, image=""):
    window = None
    max_open = 0
    closed = 0
    transition = 0
    grid_size = 0
    grid = []
    empty_case = (0, 0)
    for idx, line_not_stripped in enumerate(sys.stdin):
        line = line_not_stripped.strip()
        if idx == 0:
            grid_size = int(line)
        elif idx <= grid_size:
            grid.append(list(map(int, line.split(" "))))
        elif idx == grid_size + 1:
            continue
        elif idx == grid_size + 2:
            max_open = return_splitted(line)
            if max_open == "":
                print("Error:", line)
                return
        elif idx == grid_size + 3:
            closed = return_splitted(line)
            if closed == "":
                print("Error:", line)
                return
        elif idx == grid_size + 4:
            transition = return_splitted(line)
            if transition == "":
                print("Error:", line)
                return
        elif idx == grid_size + 5:
            empty_case = find_empty_case(grid, grid_size)
            window = PuzzleWindow(grid, max_open, closed, transition)
            if image != "":
                window.background_image(image)
            window.initialize_grid()
            if empty_case == (-1, -1):
                print("Error can't find the empty case")
                return
            window.set_speed_lvl(speed_lvl)
        else:
            direction = read_move(line)
            piece_to_move = add_piece_direction(empty_case, reverse_move(direction))
            window.move_piece(piece_to_move, direction)
            empty_case = add_piece_direction(empty_case, reverse_move(direction))

    print("max open =", max_open)
    print("closed =", closed)
    print("transitions =", transition)
    window.start()
