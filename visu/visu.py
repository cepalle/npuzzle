from tkinter import *
from colour import Color
from variables import MOVE_UP, MOVE_DOWN, MOVE_LEFT, SIZE_WINDOW, EMPTY_CASE, SPACES, SIZE_FOR_TEXT
from utils import add_piece_direction, num_to_pos, coord0
from PIL import Image, ImageTk


def crop_image(input):
    try:
        img = Image.open(input)
    except:
        return None
    img_width, img_height = img.size
    if img_width < SIZE_WINDOW or img_height < SIZE_WINDOW:
        return None
    cropped_img = img.crop(((img_width - SIZE_WINDOW) // 2,
                            (img_height - SIZE_WINDOW) // 2,
                            (img_width + SIZE_WINDOW) // 2,
                            (img_height + SIZE_WINDOW) // 2))
    return cropped_img


def split_image(im, grid_size):
    (x, y) = (0, 0)
    cropped_img = []
    img_width, img_height = im.size
    height = int(img_height / grid_size)
    width = int(img_width / grid_size)
    for i in range(0, img_height, height):
        temp = []
        for j in range(0, img_width, width):
            box = (j, i, j + width, i + height)
            a = im.crop(box)
            temp.append(ImageTk.PhotoImage(a))
        cropped_img.append(temp)
    return cropped_img



class PuzzleWindow:
    root = Tk(screenName="N-puzzle")
    puzzle = [[]]
    move_number = 0
    speed = 500
    bg_image = None
    divider_speed = 10
    _spaces = SPACES
    _grid_size = 0
    _next_action = 1500
    _move_timer = 10
    _label = []
    _text_label = None
    _cell_size = 6
    _gradient = []
    _size_of_one = 0
    _font = ("Purisa", 20, "bold")
    _max_open = 0
    _closed = 0
    _transition = 0

    def __init__(self, start_puzzle, max_open, closed, transition):
        self.puzzle = start_puzzle
        self.root.title("N-puzzle")
        geometry = SIZE_WINDOW
        self.root.geometry(str(geometry + self._spaces + SIZE_FOR_TEXT) + "x" + str(geometry + self._spaces))
        self._grid_size = len(start_puzzle[0])
        self._size_of_one = int(SIZE_WINDOW / self._grid_size)
        font_size = int(500 / self._grid_size)
        self._font = ("Purisa", font_size, "bold")
        self._gradient = list(Color("green").range_to(Color("blue"), self._grid_size * self._grid_size))
        self.__update_text__(0, max_open, closed, transition)
        self._max_open = max_open
        self._closed = closed
        self._transition = transition

    def initialize_grid(self):
        size = self._grid_size
        self._label = [[EMPTY_CASE for i in range(size)] for j in range(size)]
        for y in range(size):
            for x in range(size):
                if self.bg_image and self.puzzle[y][x] != EMPTY_CASE:
                    xa, ya = num_to_pos(self.puzzle[y][x], self._grid_size)
                    self._label[y][x] = Label(self.root, image=self.bg_image[ya][xa])
                elif self.puzzle[y][x] != EMPTY_CASE:
                    self._label[y][x] = Label(self.root, justify="center", bg=self._gradient[self.puzzle[y][x] - 1].hex,
                                              text=str(self.puzzle[y][x]), font=self._font, borderwidth=1)
                if self.puzzle[y][x] != EMPTY_CASE and type(self._label[y][x]) is not int:
                    self._label[y][x].config(width=self._cell_size, height=int(self._cell_size / 2))
                    self._label[y][x].place(x=x * self._size_of_one + self._spaces, y=y * self._size_of_one + self._spaces,
                                            width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)

    def __update_text__(self, move_nb, max_open, closed, transition):
        if self._text_label:
            self._text_label.destroy()
        text_to_display = "Max open: " + str(max_open) + "\nClosed: " + str(closed) + "\nTransitions: " +\
                          str(move_nb) + "/" + str(transition)
        self._text_label = Label(text=text_to_display, font=("Purisa", 25, "bold"))
        self._text_label.place(x=SIZE_WINDOW + self._spaces + 50, y=int(SIZE_WINDOW / 2))

    def __slow_move__(self, x, y, direction):
        divider = self.divider_speed
        move_values = []

        self.move_number += 1
        self.__update_text__(self.move_number, self._max_open, self._closed, self._transition)

        for i in range(divider - 1, -1, -1):
            move_values.append(self._size_of_one * i / divider)
        if self.bg_image:
            xa, ya = num_to_pos(self.puzzle[y][x], self._grid_size)
            self._label[y][x] = Label(self.root, image=self.bg_image[ya][xa])
        else:
            self._label[y][x] = Label(self.root, justify="center", bg=self._gradient[self.puzzle[y][x] - 1].hex,
                                      text=str(self.puzzle[y][x]), font=self._font, borderwidth=1)
        self._label[y][x].config(width=self._cell_size, height=int(self._cell_size / 2))
        for move in move_values:
            if direction == MOVE_UP and type(self._label[y][x]) is not int:
                self._label[y][x].place(x=x * self._size_of_one + self._spaces, y=y * self._size_of_one + self._spaces + move,
                                        width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)
            elif direction == MOVE_DOWN and type(self._label[y][x]) is not int:
                self._label[y][x].place(x=x * self._size_of_one + self._spaces, y=y * self._size_of_one + self._spaces - move,
                                        width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)
            elif direction == MOVE_LEFT and type(self._label[y][x]) is not int:
                self._label[y][x].place(x=x * self._size_of_one + self._spaces + move, y=y * self._size_of_one + self._spaces,
                                        width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)
            elif type(self._label[y][x]) is not int:
                self._label[y][x].place(x=x * self._size_of_one + self._spaces - move, y=y * self._size_of_one + self._spaces,
                                        width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)
            self.root.update()

    def __do_move__(self, ex_pos, direction):
        new_pos = add_piece_direction(ex_pos, direction)
        (ex_x, ex_y) = ex_pos
        (x, y) = new_pos
        self.puzzle[y][x] = self.puzzle[ex_y][ex_x]
        self.puzzle[ex_y][ex_x] = EMPTY_CASE
        if type(self._label[ex_y][ex_x]) is not int:
            self._label[ex_y][ex_x].destroy()
        self.__slow_move__(x, y, direction)
        self._label[ex_y][ex_x] = EMPTY_CASE

    def __finish_image__(self):
        (x, y) = coord0(self._grid_size)
        finish_image = Label(self.root, image=self.bg_image[y][x])
        finish_image.config(width=self._cell_size, height=int(self._cell_size / 2))
        finish_image.place(x=x * self._size_of_one + self._spaces, y=y * self._size_of_one + self._spaces,
                                width=self._size_of_one - self._spaces, height=self._size_of_one - self._spaces)

    def set_speed_lvl(self, speed_lvl):
        print("speed_lvl", speed_lvl)
        if speed_lvl == 1:
            self.speed = 500
            self.divider_speed = 8
        elif speed_lvl == 2:
            self.speed = 400
            self.divider_speed = 4
        elif speed_lvl == 3:
            self.speed = 300
            self.divider_speed = 3
        elif speed_lvl == 4:
            self.speed = 200
            self.divider_speed = 2
        elif speed_lvl == 5:
            self.speed = 100
            self.divider_speed = 1

    def move_piece(self, piece, direction):
        self.root.after(self._next_action, self.__do_move__, piece, direction)
        self._next_action += self.speed

    def background_image(self, path_to_image):
        cropped_image = crop_image(path_to_image)
        if cropped_image is None:
            self.bg_image = None
            print("Error while opening the image")
            return
        self.bg_image = split_image(cropped_image, self._grid_size)
        if self.bg_image:
            self._spaces = 0

    def start(self):
        if self.bg_image:
            self.root.after(self._next_action + 1800, self.__finish_image__)
        self.root.mainloop()
