import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import matplotlib as mpl
import itertools
import math


class Color:
    black = 'k'
    white = 'w'


class Field:
    def __init__(self, width, height):
        self.width = width
        self.height = height


class Direction:
    left = [-1, 0]
    right = [1, 0]
    top = [0, 1]
    down = [0, -1]


class Ant:
    def __init__(self, Field=Field(50, 50), max_steps=math.inf):
        self.width = Field.width
        self.height = Field.height
        self.x = self.width // 2
        self.y = self.height // 2
        self.max_steps = max_steps
        self.positions = []

    def __invert_color(self, grid, x, y):
        if grid[y][x] == Color.black:
            grid[y][x] = Color.white
            return ""
        grid[y][x] = Color.black
        return ""

    def __next_direction(self, grid, x, y, direction):
        if grid[y][x] == Color.white:
            order = [Direction.left, Direction.down,
                     Direction.right, Direction.top]
        else:
            order = [Direction.left, Direction.top,
                     Direction.right, Direction.down]

        index = order.index(direction)

        if index < len(order) - 1:
            index += 1
        else:
            index = 0

        return order[index]

    def __next_position(self, x, y, direction):
        x += direction[0]
        y += direction[1]
        return [x, y]

    def __display_plot(self, positions, width, height):
        plt.style.use('seaborn-notebook')

        xdata = [position[0] for position in positions]
        ydata = [position[1] for position in positions]

        _, ax = plt.subplots()

        ax.set_xlim(0, width)
        ax.set_ylim(0, height)

        colors = itertools.cycle([Color.black, Color.white])

        for x, y in zip(xdata, ydata):
            plt.scatter(x, y, color=next(colors), s=1,
                        marker='s', linewidths=2)

        plt.show()
        return ""

    def start(self):
        grid = [[Color.white] * self.width for _ in range(self.height)]
        direction = Direction.left
        steps_counter = 0

        while 0 < self.x < self.width and 0 < self.y < self.height and steps_counter <= self.max_steps:
            self.x, self.y = self.__next_position(self.x, self.y, direction)
            position = [self.x, self.y]
            self.positions.append(position)
            direction = self.__next_direction(grid, self.x, self.y, direction)
            self.__invert_color(grid, self.x, self.y)
            steps_counter += 1

        self.__display_plot(self.positions, self.width, self.height)
        return self.positions


field = Field(80, 80)
ant = Ant(field, 11000)
print(ant.start())
