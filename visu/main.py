import argparse
from reader import read_input

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Visualization of a npzuzzle solver.')
    parser.add_argument('-s', '--speed', help="Speed of the visualization from 1 (min speed) to 5 (max speed).", type=int, default=0)
    parser.add_argument('-i', '--image', help="Image path instead of numbers.", default="")
    args = parser.parse_args()
    read_input(args.speed, args.image)
