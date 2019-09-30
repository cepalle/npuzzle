from PIL import Image, ImageTk

def add_piece_direction(piece, direction):
    return tuple(map(lambda x, y: x + y, piece, direction))


def reverse_move(move):
    (x, y) = move
    return -x, -y


def coord0(grid_size):
    return int((grid_size - 1) / 2), int(grid_size / 2)


def num_to_pos(num, grid_size):
    if num > grid_size * 4 - 4:
        (x, y) = num_to_pos(num - (grid_size * 4 - 4), grid_size - 2)
        return x + 1, y + 1
    else:
        if num <= grid_size:
            return num - 1, 0
        elif num <= grid_size * 2 - 1:
            return grid_size - 1, num - grid_size
        elif num <= grid_size * 3 - 2:
            return grid_size * 3 - 2 - num, grid_size - 1
        else:
            return 0, grid_size * 4 - 3 - num

